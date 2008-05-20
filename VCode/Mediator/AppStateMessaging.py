##############################################################################
# VoiceCode, a programming-by-voice environment
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# (C) 2001, National Research Council of Canada
#
##############################################################################

"""Application state for an external editor communicating through a messaging protocol."""


import debug, sys
from Object import Object

import AppState, AppStateCached, messaging, SourceBuffMessaging


        
class AppStateMessaging(AppStateCached.AppStateCached):
    
    """Application state for an external editor communicating through
    a messaging protocol.

    **INSTANCE ATTRIBUTES**
    
    STR *id* -- Unique identifier for the external application instance.

    [Messenger] *listen_msgr* -- Messenger used to listen for commands
    from external application.

    [Messenger] *talk_msgr* -- Messenger used to send commands
    to external application. 

    *BOOL listen_can_block* -- flag indicating that the listen_msgr to
    can block on get_mess if there is no message waiting.  This flag is
    provided only for compatibility with ServerSingleThread.  All other
    servers should be set up so that listen_msgr avoids blocking and 
    simply return None if there are no messages.

    BOOL *closing* -- flag indicating that the external editor has
    told us it is disconnecting, or the listen socket connection has
    been broken, or the AppMgr has called cleanup

    BOOL *multiple_buffer_support* -- does the editor support multiple
    open buffers?

    BOOL *bidirectional_selection_support* -- does the editor support 
    bidirectional selection (cursor can be at either end of the
    selection)

    BOOL *in_shared_window* -- is the editor running in a potentially
    shareable window (i.e. a shell window where the editor can be
    suspended or closed without closing the window)

    BOOL *multiple_window_support* -- does the editor open multiple
    windows?

    BOOL *is_active_safe* -- an internal flag indicating whether it is
    safe to query the editor to see if it is active, or whether doing so
    when the editor is inactive will block

    BOOL *activity_state* -- an internal flag to keep track of whether
    the editor process is active or suspended

    BOOL *query_active* -- if false, the activity_state is accurate, and
    we should use it to respond to is_active.  If true, we instead 
    query the editor

    STR *the_instance_string* -- unique in identifying string to be 
    included in the window title if possible.

    BOOL *can_show_instance_string* -- flag indicating whether the editor 
    can and will include the instance string in the title of every 
    window containing an editor buffer.

    **CLASS ATTRIBUTES**
    

    .. [Messenger] file:///messaging.Messenger.html"""

    
    def __init__(self, listen_msgr=None, talk_msgr=None, id=None, 
        listen_can_block = 0, **attrs):
        self.init_attrs({'multiple_buffer_support' : 0,
            'bidirectional_selection_support' : 0,
            'in_shared_window' : 0,
            'multiple_window_support': 0})        
        self.deep_construct(AppStateMessaging, 
                            {'id': id,
                             'the_instance_string': None,
                             'can_show_instance_string': 0,
                             'listen_msgr': listen_msgr,
                             'listen_can_block': listen_can_block,
                             'talk_msgr': talk_msgr,
                             'closing': 0,
                             'is_active_safe': 0,
                             'activity_state': 1,
                             'query_active': 0
                            },
                            attrs)
        if self.suspendable():
            if self.suspend_notification():
# suspendable, but will notify us.  Therefore, we should maintain and
# update an internal activity_state, and use that when is_active is
# called, rather than querying the application
                self.query_active = 0
                self.is_active_safe = 1
            else:
# If the editor can't notify us, then we have to query it, but doing so
# is unsafe (i.e. it may block)
                self.query_active = 1
                self.is_active_safe = 0
        else:
# not suspendable, so it should be safe to query the editor to see if it
# is active
            self.query_active = 1
            self.is_active_safe = 1
        self.activity_state = self._is_active_from_app()
        self.in_shared_window =  self._shared_window_from_app()
        self.multiple_window_support =  self._multiple_windows_from_app()
        self.multiple_buffer_support =  self._multiple_buffers_from_app()
        self.bidirectional_selection_support = \
            self._bidirectional_selection_from_app()
        self.init_cache()


    def remove_other_references(self):
        self.closing = 1
        AppStateCached.AppStateCached.remove_other_references(self)

    def new_compatible_sb(self, buff_name):
        """Creates a new instance of [SourceBuff].

        Note: The class used to instantiate the [SourceBuff] needs to
        be compatible with the class of *self*. With a few exceptions
        (if any), each subclass of *AppState* will have to redefine
        *new_compatible_sb* in order to generate a [SourceBuff] of the
        appropriate class.
        
        **INPUTS**
                
        STR *buff_name* -- unique name of the source buffer.
        
        **OUTPUTS**
        
        *none* -- 

        ..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""
        
        return SourceBuffMessaging.SourceBuffMessaging(app=self, buff_name=buff_name)


    def config_from_external(self):
        
        """Lets the external editor configure the *AppStateMessaging*.

        Configuration is done through messages on the connection. The
        messages may vary from editor to editor.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        debug.virtual('AppStateMessaging.config_from_external')

    def recog_begin(self, window_id, block = 0):
        """Invoked at the beginning of a recognition event.

        The editor then returns telling VoiceCode whether or not the user
        is allowed to speak into window *window_id*.

        **INPUTS**
        
        INT *window_id* -- The ID of the window that was active when
        the recognition began.                

        *BOOL block* -- true if the speech engine can detect recog_end
        events reliably.  If so, and if the editor is capable of doing so, 
        the editor may (at its discretion) also stop responding to user
        input until method [recog_end()] is invoked.  This is to
        prevent a bunch of problems that can arise if the user types
        while VoiceCode is still processing an utterance. In such
        cases, the results of the utterance interpretation can be
        unpredictable, especially when it comes to correction.

        **NOTE:** However, if block is false, the editor **MUST NOT**
        stop responding, because the mediator will not be able to use
        recog_end to tell it to resume responding to user input.  

        Also, the editor must provide a way for the user to re-enable
        input manually, in case the mediator crashes.  If it cannot do
        so, it should not stop responding, regardless of the value of
        block.

        **OUTPUTS**
        
        BOOL *can_talk* -- *true* iif editor allows user to speak into window
        with ID *window_id*
        
        .. [recog_end()] file:///./AppState.AppState.html#recog_end"""

        self.talk_msgr.send_mess('recog_begin', {'window_id': window_id, 
            'block': block})
        response = self.talk_msgr.get_mess(expect=['recog_begin_resp'])
        return messaging.messarg2int(response[1]['value'])

    def recog_end(self):
        """Invoked at the end of a recognition event.

        This tells the editor to start responding to user
        input again, and possibly to execute any user inputs it may
        have recorded since [recog_begin()] was invoked.
        
        Each external editor will respond to that message as best it can.

        Ideally, the editor would:

        - Execute all actions that were logged
        
        - Stop recording user actions to a log, and execute them as
          they arrrive instead.
        
        If the editor is able to stop responding to user input, but is
        not able to record them and/or execute them later, then it
        should:

        - Start responding to user input again

        If the editor is not even able to stop responding to user
        input, then it should:

        - Do nothing

        NOTE: This method may be never be invoked

        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 

        ..[recog_begin()] file:///./AppState.AppState.html#recog_begin"""
        
        self.talk_msgr.send_mess('recog_end')
        response = self.talk_msgr.get_mess(expect=['recog_end_resp'])

    def mediator_closing(self):
        """method called to inform AppState that the mediator is
        closing.    Internal editors should exit.  They may prompt the
        user to save modified files, but must not allow the user to
        cancel and leave the editor running.  External editors should
        disconnect but not close.  **Note:** this method should not
        block.  For external editors, that means the corresponding
        message should have a response for which to wait.  Otherwise, a
        single hung or disconnected editor hang the mediator and prevent
        it from closing or from notifying the rest of the connected
        editors that it was closing.  

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.talk_msgr.send_mess('mediator_closing')
# this message has no response, otherwise we might block waiting for it
# if the external editor had hung, crashed, or been disconnected

    def listen_one_transaction(self):
        """Completes a single editor-initiated transaction
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *BOOL* -- true if an update message was read successfully, and
        it was not an editor_disconnecting or connection_broken message.
        This return value is used by process_pending_updates to
        determine if it should call listen_one_transaction again.
        """
        debug.trace('AppStateMessaging.listen_one_transaction', 'called')
        expected = ['updates', 'suspended', 'resuming',
            'editor_disconnecting', 'connection_broken']
        mess = self.listen_msgr.get_mess(expect= expected)
        if mess == None:
            return 0
        debug.trace('AppStateMessaging.listen_one_transaction', 
            'heard %s' % repr(mess))
        mess_name = mess[0]
        debug.trace('AppStateMessaging.listen_one_transaction', 
            'heard "%s"' % mess_name)
        if mess_name == 'updates':
            debug.trace('AppStateMessaging.listen_one_transaction', 
                'was updates')
            mess_cont = mess[1]
            debug.trace('AppStateMessaging.listen_one_transaction', 
                'content was %s' % mess_cont)
            upd_list = mess_cont['value']
            debug.trace('AppStateMessaging.listen_one_transaction', 
                'updates %s' % str(upd_list))
            self.apply_updates(upd_list)
            return 1
        elif mess_name == 'suspended':
            self.activity_state = 0
            self.suspend_cbk()
# there could be a resuming message in the queue, followed by others, so
# we should still return true
            return 1
        elif mess_name == 'resuming':
            self.activity_state = 1
            self.resume_cbk()
            return 1
        elif mess_name == 'editor_disconnecting':
            debug.trace('AppStateMessaging.listen_one_transaction',
                'received editor_disconnecting')
            if not self.closing:
              self.closing = 1
              self.close_app_cbk()
            return 0
        elif mess_name == 'connection_broken':
            debug.trace('AppStateMessaging.listen_one_transaction',
                'data thread sent connection broken')
            if not self.closing:
              self.closing = 1
              self.close_app_cbk(unexpected = 1)
            return 0
        else:
            debug.trace('AppStateMessaging.listen_one_transaction', 
                'bad message')
            return 1

    def process_pending_updates(self):
        """Process any pending updates which the editor has already
        sent us, before querying the editor for additional updates.
        
        **INPUTS**

        *none*
        
        **OUTPUTS**
        
        *none* 
        """
        if not self.listen_can_block:
# read messages until the queue is empty
            while self.listen_one_transaction():
                pass

    def updates_from_app(self, what = None, exclude=1):
        """Gets a list of updates from the external app.

        Does this through a messaging protocol.
        
        Note: the list of updates must ALWAYS include the name of the
        external app's active buffer.
        
        **INPUTS**
        
        [STR] *what* -- List of items to be included/excluded in the updates.

        BOOL *exclude* -- Indicates if *what* is a list of items to be
        included or excluded from updates.
        
        **OUTPUTS**
        
        [ [AS_Update] ] *updates* -- List of updates retrieved from the
        external app.
        
        ..[AS_Update] file:///./AppState.AS_Update.html"""

        debug.trace('AppStateMessaging.updates_from_app', 'invoked')
        if what == None:
            what = []
        self.talk_msgr.send_mess('updates')
        response = self.talk_msgr.get_mess(expect=['updates'])

        #
        # Parse response as a list of udate messages
        #
        return response[1]['value']

    def apply_upd_descr(self, upd_descr_list):
        
        """Applies a updates provided by a list of update descriptions.
        
        **INPUTS**
        
        [{STR: ANY}] *upd_descr_list* -- List of update descriptions
        

        **OUTPUTS**
        
        [ AS_Update ] -- the list of update objects

        ..[AS_Update] file:///./AppState.AS_Update.html"""

        debug.trace('AppStateMessaging.apply_upd_descr', 'upd_descr_list=%s' % repr(upd_descr_list))
        updates = []
        for a_descr in upd_descr_list:
            the_update = AppState.create_update(a_descr)
            updates.append(the_update)
            debug.trace('AppStateMessaging.apply_upd_descr', 'the_update=%s' % repr(the_update))
            the_update.apply(self)
        return updates

    def app_active_buffer_name(self):
        
        """Reads the file name of the active buffer, directly from the
        external application.

        **OUTPUTS**

        *STR* -- file name of app's active buffer"""

        self.talk_msgr.send_mess('active_buffer_name')
        response = self.talk_msgr.get_mess(expect=['active_buffer_name_resp'])
        return response[1]['value']                

    def is_active(self):
        """is the editor application active (not suspended)?

        Usually true, except for remote editors running in a (Unix)
        shell.  GUI editors tend to minimize instead of suspending, so
        their process should still be active.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor is active (i.e. has not been suspended)
        """
        if self.query_active:
            return self._is_active_from_app()
        else:
# must make sure that there are no unprocessed 'suspended' or 
# 'resuming' messages in the queue
            self.process_pending_updates()
            if self.closing:
                return 0
            return self.activity_state

    def _is_active_from_app(self):
        """private method to query the editor application to see if its
        process is active (not suspended)?

        Usually true, except for remote editors running in a (Unix)
        shell.  GUI editors tend to minimize instead of suspending, so
        their process should still be active.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor is active (i.e. has not been suspended)
        """
        self.talk_msgr.send_mess('process_active')
        response = self.talk_msgr.get_mess(expect=['process_active_resp'])
        return response[1]['value']                

    def is_active_is_safe(self):
        """can is_active safely be queried, without blocking?

        For example, Emacs provides a suspend-hook and a
        suspend-resume-hook, so a properly written AppStateEmacs can
        set a flag on suspend and clear it on resume, and will therefore
        be able to respond to is_active without querying Emacs.

        Also, except for remote editors running in a (Unix)
        shell, this is usually true.  GUI editors tend to minimize 
        instead of suspending, so their process should still be active.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if is_active can be queried without blocking,
        even if the editor has been suspended. 
        """
        return self.is_active_safe

    def suspendable(self):
        """is the editor running in an environment where it can be suspended?
        (if, e.g., it was started from a Unix command-line, except for 
        GUI editors which fork, allowing the command-line command to exit).  
        If so, this makes querying the editor to is if it is_active unsafe. 

        Usually false for Windows and most GUI editors.

        **NOTE:** this method is used to determine how to implement
        is_active and whether is_active_is_safe.  It is generally 
        called only by an AppState subclass (or a ClientEditor wrapper) 
        and only when the editor first starts or connects to the mediator.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor is running in an environment where 
        it can be suspended
        """
        self.talk_msgr.send_mess('suspendable')
        response = self.talk_msgr.get_mess(expect=['suspendable_resp'])
        return response[1]['value']                

    def suspend_notification(self):
        """does the editor supports suspend notification?

        **NOTE:** this method is used to determine how to implement
        is_active and whether is_active_is_safe.  It is generally 
        called only by an AppState subclass (or a ClientEditor wrapper) 
        and only when the editor first starts or connects to the mediator.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if the editor can (and will) notify the mediator
        prior to its process being suspended and once it has been resumed.
        """
        self.talk_msgr.send_mess('suspend_notification')
        response = self.talk_msgr.get_mess(expect=['suspend_notification_resp'])
        return response[1]['value']                

    def shared_window(self):
        """is the editor running in a window which could be shared with
        another editor instance (because it is a shell window,
        and this instance could be suspended or closed)

        Usually false for GUI editors.

        Note: remote editors running in a remote display
        which appears as a single window to be local operating system 
        (X servers in single window mode, VNC) will also appear to be
        shared windows.  However, the mediator will perform a separate 
        check to detect this, so for remote editors which do not share windows 
        on the remote system, AppState.shared_window should report
        false.
        
        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor is running in a potentially shared window
        """
        return self.in_shared_window

    def _shared_window_from_app(self):
        """is the editor running in a window which could be shared with
        another editor instance (because it is a shell window,
        and this instance could be suspended or closed)

        Usually false for GUI editors.

        Note: remote editors running in a remote display
        which appears as a single window to be local operating system 
        (X servers in single window mode, VNC) will also appear to be
        shared windows.  However, the mediator will perform a separate 
        check to detect this, so for remote editors which do not share windows 
        on the remote system, AppState.shared_window should report
        false.
        
        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor is running in a potentially shared window
        """
        self.talk_msgr.send_mess('shared_window')
        response = self.talk_msgr.get_mess(expect=['shared_window_resp'])
        return response[1]['value']                

    def set_instance_string(self, instance_string):
        """specifies the identifier string for this editor instance.  If the 
        editor is capable of setting the window title to include this string, 
        it should (and then should return this string when the
        instance_string method is called.  

        **INPUTS**

        *STR* instance_string -- the identifying string to be included in the
        window title if possible.

        **OUTPUTS**
        
        *BOOL* -- true if the editor can and will include the 
        instance string in its window title for all windows 
        containing editor buffers.
        """
        self.talk_msgr.send_mess('set_instance_string', 
            {'instance_string': instance_string})
        response = self.talk_msgr.get_mess(expect = \
            ['set_instance_string_resp'])
        self.the_instance_string = instance_string
        self.can_show_instance_string = response[1]['value']
        return self.can_show_instance_string 

    def instance_string(self):
        """returns the identifier string for this editor instance (which 
        should be a substring of the window title)

        Note: multiple windows of remote editors running in a remote display
        which appears as a single window to be local operating system 
        (X servers in single window mode, VNC) will not be able to set 
        the overall title.  
        However, the mediator will perform a 
        separate check to detect this, so remote editors which support
        identifying title strings should still return the appropriate
        string.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *STR* -- the identifying string, or None if the editor was not given 
        such a string or cannot set the window title.
        """
        if self.can_show_instance_string:
            return self.the_instance_string
        return None

    def title_escape_sequence(self, before = "", after = ""):
        """gives the editor a (module-dependent) hint about the escape
        sequence which can be used to set the module's window title, if
        any.  If the editor has its own mechanism for setting the window
        title, it should simply ignore this method.  

        **INPUTS**

        *STR* before -- the escape sequence to be sent before the string
        to place in the window title, or the empty string if there is no
        escape sequence

        *STR* after -- the escape sequence which terminates the window
        title value

        **OUTPUTS**

        *BOOL* -- true if the editor, given the title escape sequence, 
        can and will include the instance string in its window title 
        for all windows containing editor buffers.
        """
        if before != "" and after != "":
            self.talk_msgr.send_mess('title_escape', 
                {'before': before, 'after': after})
            response = self.talk_msgr.get_mess(expect = ['title_escape_resp'])
            self.can_show_instance_string = response[1]['value']
        return self.can_show_instance_string

    def multiple_windows(self):
        """does editor support multiple windows per instance?

        Note: the purpose of this function is to allow the RecogStartMgr
        to determine whether a previously unknown window could belong to
        this known instance.  Therefore, Emacs running in text mode 
        should return false, even though it can have (sub-)windows in 
        a single frame.  
        
        Note: multiple windows of remote editors running in a remote display
        which appears as a single window to be local operating system 
        (X servers in single window mode, VNC) will not appear to the mediator 
        as having separate windows.  However, the mediator will perform a 
        separate check to detect this, so remote editors which support
        multiple windows should return true, regardless of the remote
        display method.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports opening multiple editor windows.  
        """
        return self.multiple_window_support

    def _multiple_windows_from_app(self):
        """does editor support multiple windows per instance?

        Note: the purpose of this function is to allow the RecogStartMgr
        to determine whether a previously unknown window could belong to
        this known instance.  Therefore, Emacs running in text mode 
        should return false, even though it can have (sub-)windows in 
        a single frame.  
        
        Note: multiple windows of remote editors running in a remote display
        which appears as a single window to be local operating system 
        (X servers in single window mode, VNC) will not appear to the mediator 
        as having separate windows.  However, the mediator will perform a 
        separate check to detect this, so remote editors which support
        multiple windows should return true, regardless of the remote
        display method.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports opening multiple editor windows.  
        """
        self.talk_msgr.send_mess('multiple_windows')
        response = self.talk_msgr.get_mess(expect=['multiple_windows_resp'])
        return response[1]['value']                

    def multiple_buffers(self):
        """does editor support multiple open buffers?

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports having multiple buffers open 
        at the same time"""

        return self.multiple_buffer_support
        
    def _multiple_buffers_from_app(self):
        """does editor support multiple open buffers?

        Retrieve this information directly from the external editor.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports having multiple buffers open 
        at the same time"""

        self.talk_msgr.send_mess('multiple_buffers')
        response = self.talk_msgr.get_mess(expect=['multiple_buffers_resp'])
        return response[1]['value']                
        
    def bidirectional_selection(self):
        """does editor support selections with cursor at left?

        Get this value directly from the external editor

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor allows setting the selection at the
        left end of the selection"""

        return self.bidirectional_selection_support

    def _bidirectional_selection_from_app(self):
        """does editor support selections with cursor at left?

        Get this value directly from the external editor

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor allows setting the selection at the
        left end of the selection"""

        self.talk_msgr.send_mess('bidirectional_selection')
        response = self.talk_msgr.get_mess(expect=['bidirectional_selection_resp'])
        return response[1]['value']                


    def tell_editor_to_open_file(self, file_name):
        """Tell the external editor to open a file.

        STR *file_name* -- The full path of the file to be opened.
        
        **OUTPUTS**
        
        STR *buff_name* -- Unique name of the buffer in which the file
        was opened.

        """

#        print '-- AppStateMessaging.tell_editor_to_open_file: name=%s' % name
        
        #
        # Tell external editor to open the file
        #
        self.talk_msgr.send_mess('open_file', {'file_name': file_name})
        response = self.talk_msgr.get_mess(expect=['open_file_resp'])
        buff_name = response[1]['buff_name']

        return buff_name

    def app_save_file(self, full_path = None, no_prompt = 0):
        """Tell the external editor to save the current buffer.

        **INPUTS**
        
        *STR full_path* -- full path under which to save the file, or
        None to use the buffer name

        *BOOL no_prompt* -- overwrite any existing file without
        prompting.  No_prompt should only be set to true if the caller
        has already prompted the user.

        **OUTPUTS**

        *STR* -- new buffer name if successful, or None if the save 
        failed
        """
        #
        # Tell external editor to save the file
        #
        self.talk_msgr.send_mess('save_file', 
            {'full_path': full_path,
             'no_prompt': no_prompt
            })
        response = self.talk_msgr.get_mess(expect=['save_file_resp'])
        buff_name = response[1]['buff_name']

        return buff_name
        
        
    def query_buffer_from_app(self, buff_name):
        """query the application to see if a buffer by the name of buff_name 
        exists.

        **INPUTS**

        *STR* buff_name -- name of the buffer to check

        **OUTPUTS**

        *BOOL* -- does the buffer exist?
        """
        self.talk_msgr.send_mess('confirm_buffer_exists', {'buff_name': buff_name})
        response = \
            self.talk_msgr.get_mess(expect=['confirm_buffer_exists_resp'])
        buffer_exists = response[1]['value']
        return buffer_exists

    def open_buffers_from_app(self):
        """retrieve a list of the names of open buffers from the
        application.

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of the names of open buffers
        """
        self.talk_msgr.send_mess('list_open_buffers')
        response = \
            self.talk_msgr.get_mess(expect=['list_open_buffers_resp'])
        open_buffers = response[1]['value']
        return open_buffers

    def app_close_buffer(self, buff_name, save=0):
        """Ask the editor to close a buffer.
        
        **INPUTS**
        
        STR *buff_name* -- name of buffer to close
        
        INT *save* -- *-1* -> don't save the buffer
                            *0* -> query user if buffer needs saving
                            *1* -> save without querying user
        

        **OUTPUTS**
        
        *BOOL* -- true if the editor does close the buffer
        """

        self.talk_msgr.send_mess('close_buffer', {'buff_name': buff_name, 'save': save})
        response = self.talk_msgr.get_mess(expect=['close_buffer_resp'])
        success = response[1]['value']
        return success


class AppStateInsertIndentMess(AppStateMessaging):
    
    """subclass of AppStateMessaging which uses
    SourceBuffInsertIndentMess in place of SourceBuffMessaging

    **NOTE:** This class is used only for test editors.  Real editors 
    supporting client-side indentation should use SourceBuffMessaging.  
    Real editors not supporting client-side indentation should use 
    server-side indentation (see SB_MessExtEdSim in tcp_server.py 
    for an example).

    Its purpose is to work with clients with an incomplete implementation 
    of client-side indentation which won't work with the generic 
    AppState.insert_indent, because indent is implemented as a no-op.
    

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**
    
    *none*
    """

    
    def __init__(self, **attrs):
        self.deep_construct(AppStateInsertIndentMess, 
                            {},
                            attrs)

    def new_compatible_sb(self, buff_name):
        """Creates a new instance of [SourceBuff].

        Note: The class used to instantiate the [SourceBuff] needs to
        be compatible with the class of *self*. With a few exceptions
        (if any), each subclass of *AppState* will have to redefine
        *new_compatible_sb* in order to generate a [SourceBuff] of the
        appropriate class.
        
        **INPUTS**
                
        STR *buff_name* -- unique name of the source buffer.
        
        **OUTPUTS**
        
        *none* -- 

        ..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""
        
        return SourceBuffMessaging.SourceBuffInsertIndentMess(app=self, 
            buff_name=buff_name)

