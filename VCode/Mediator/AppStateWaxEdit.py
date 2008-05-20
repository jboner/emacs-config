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
# (C)2000, National Research Council of Canada
#
##############################################################################

"""AppState wrapper over the simple pythoon-based GUI editor (WaxEdit)."""

import os, posixpath, re, sys
import auto_test, debug
import AppState, AppStateNonCached, as_services
from SourceBuffTB import SourceBuffTB

class AppStateWaxEdit(AppStateNonCached.AppStateNonCached):
    """This class is a an AppState wrapper on top of WaxEdit.

    It is used to decouple from any external editor so that we can
    test it without resorting to the IPC infrastructure for
    communicating with external editors.

    Instead of an external editor, we use WaxEdit, a simple editor written
    in Python.
    
    **INSTANCE ATTRIBUTES**

    *WaxEdit* the_editor -- The WaxEdit editor wrapped into *self*.

    *STR* active_buffer_name -- name of the currently active buffer

    [AS_ServiceBreadcrumbs] breadcrumbs_srv -- Breadcrumbs service used by
    this AppState.
    
    **CLASS ATTRIBUTES**
    
    *none* -- 

    ..[AS_ServiceBreadcrumbs] file:///./AppState.AS_ServiceBreadcrumbs.html"""
    
    buffer_methods = AppState.AppState.buffer_methods[:]
    buffer_methods.append('print_buff')
    
    def __init__(self, editor, **attrs):
        self.deep_construct(AppStateWaxEdit,
                            {'the_editor': editor, 
                             'the_instance_string': None,
                             'can_show_instance_string': 0,
                             'active_buffer_name' : "",
                             'breadcrumbs_srv': as_services.AS_ServiceBreadcrumbs(self)},
                            attrs, new_default = {'app_name': 'WaxEdit'}
                            )
        self.add_owned('breadcrumbs_srv')
        self.open_buffers[self.active_buffer_name] =  \
            SourceBuffTB(app = self, buff_name="", \
            underlying_buffer = self.the_editor.editor_buffer(),
            language=None, change_specification = 1)

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
        
        return SourceBuffTB.SourceBuffTB(app=self, buff_name=buff_name,
            change_specification = 1)

        
    def recog_begin(self, window_id, block = 0):
        
        """Haven't figured out how to make WaxEdit block user input"""

        return 1

    def recog_end(self):
        
        """Haven't figured out how to make WaxEdit block user input"""

        pass

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
        self.close_all_buffers()

    def updates_from_app(self, what=[], exclude=1):
        
        """For AppStateWaxEdit, no need to get updates from external editor.

        We always get the state from EdSim directly, and every EdSim
        command that writes to the buffers will update the V-E map
        directly.
        
        **INPUTS**
        
        [STR] *what* -- List of items to be included/excluded in the updates.

        BOOL *exclude* -- Indicates if *what* is a list of items to be
        included or excluded from updates.
        
        **OUTPUTS**
        
        [ [AS_Update] ] *updates* -- List of updates retrieved from the
        external app.
        
        ..[AS_Update] file:///./AppState.AS_Update.html"""
        
        return []


    def app_active_buffer_name(self):
        
        """Returns the file name of the buffer currently active in the
        external application.

        Note that this may or may not be the same the buffer that
        VoiceCode is currently bound to (see [curr_buffer_name]
        method for a description of buffer binding).

        **INPUTS**

        *none* --
        
        **OUTPUTS**

        *STR* -- file name of current buffer

        file:///./AppState.AppState.html#curr_buffer_name"""        
      
        return self.active_buffer_name

    def app_change_buffer(self, buff_name):
        """Changes the external application's active buffer.

        This variant only changes the buffer in the external
        application. It does not resynchronise VoiceCode with external
        application.

        This should NOT bind the *AppState* to the new buffer. This
        should be done only by [change_buffer].
        See [curr_buffer_name] for a description of buffer binding.

        **INPUTS**
        
        STR *buff_name=None* -- Name of the buffer to switch to.
       
        **OUTPUTS**
        
        *BOOL* -- true if buff_name exists and the external application
        successfully switches to it
        
            
        file:///./AppState.AppState.html#curr_buffer_name"""

        if self.query_buffer_from_app(buff_name):
            self.active_buffer_name = buff_name
            return 1
        return 0
        
    def active_field(self):
        """indicates what part of the editor has the focus.

        **INPUTS**

        *none*

        **OUTPUTS**

        *(STR)* -- Name of the active Field. Elements of
        the array refer to a sequence of objects in the user interface
        that lead to the active field.

        If *None*, then the buffer [self.curr_buffer] has the focus. 

        Example: in VisualBasic, it might be: *('menu bar', 'File', 'Save
        as', 'file name')*.

        Example: in Emacs, it might be *('find-buffer', 'buffer-name')*
        where find-buffer is the name of the command that was invoked and
        buffer-name refers to the argument that is being asked for.
        """
        if not self.the_editor.is_active():
            return ('inactive')
        if self.the_editor.editor_has_focus():
            return None
        return ('unknown')

    def drop_breadcrumb(self, buff_name=None, pos=None):

        """Drops a breadcrumb

        *INT pos* is the position where to drop the crumb. *STR
         buff_name* is the name of the source buffer.
        
        If *pos* not specified, drop breadcrumb at cursor position.

        If *buff* not specified either, drop breadcrumb in current buffer
        """
        self.breadcrumbs_srv.drop_breadcrumb(buff_name, pos)


    def pop_breadcrumbs(self, num=1, gothere=1):
        """Pops breadcrumbs from the breadcrumbs stack

        *INT num* is the number of crumbs to pop. If None, then pop 1 crumb.

        if *BOOL gothere* is true, then move cursor to the last popped
        breadcrumb.
        """
        self.breadcrumbs_srv.pop_breadcrumbs(num, gothere)




    def tell_editor_to_open_file(self, file_name):
        """See [AppState.tell_editor_to_open_file()] for doc.

        ..[AppState.tell_editor_to_open_file()] file:///./AppState.AppState.html#tell_editor_to_open_file"""


        buff_name = None                
        path, short = os.path.split(file_name)
        if path:
            self.curr_dir = path
        else:
            path = self.curr_dir
            file_name = os.path.join(path, short)
        success = self.the_editor.open_file_in_buffer(file_name)
        
        # WaxEdit only supports one open buffer at a time
        if success:
            if self.curr_buffer_name() != None:
                name = self.curr_buffer_name()
                self.close_buffer(name, 0)
            self.open_buffers[file_name] = SourceBuffTB(app = self, 
                buff_name=file_name, 
                underlying_buffer = self.the_editor.editor_buffer(),
                indent_level=3, indent_to_curr_level=1,
                change_specification = 1)
            self.active_buffer_name = file_name
            self.the_editor.set_name(short)
            buff_name = self.active_buffer_name

        return buff_name

    def query_buffer_from_app(self, buff_name):
        """query the application to see if a buffer by the name of buff_name 
        exists.

        **INPUTS**

        *STR* buff_name -- name of the buffer to check

        **OUTPUTS**

        *BOOL* -- does the buffer exist?
        """
        return buff_name in self.open_buffers_from_app()

    def open_buffers_from_app(self):
        """retrieve a list of the names of open buffers from the
        application.

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of the names of open buffers
        """
        return self.open_buffers.keys()

        
    def app_save_file(self, full_path = None, no_prompt = 0):
        """Save the current buffer.

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
        f_path = full_path
        quiet = no_prompt
        if not f_path:
            f_path = self.curr_buffer_name()
            quiet = 1
        if not quiet and f_path != self.curr_buffer_name():
            if not os.path.exists(f_path):
                quiet = 1
        success = self.the_editor.save_file(f_path, quiet)
#         try:
#             source_file = open(name, 'rw')
#             source = source_file.read()
#             source_file.close()
#         except Exception, err:
#            return
        # WaxEdit only supports one open buffer at a time
        if not success:
            return None
        path, short = os.path.split(f_path)
        if path:
            self.curr_dir = path
        old_name = self.curr_buffer_name()
        if not old_name or old_name != f_path:
            self.active_buffer_name = f_path
# buffer has been renamed.  add a new reference to the open_buffers map,
# and then delete the old one
            self.open_buffers[f_path] = self.open_buffers[old_name]
            del self.open_buffers[old_name]
        self.the_editor.set_name(short)
        return f_path
      
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
        return 1

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
        return 1

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
        return 0

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
        self.the_instance_string = title
        self.can_show_instance_string = \
            self.the_editor.set_instance_string(title)
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
# we can set the title ourselves, so ignore
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
        return 0

    def multiple_buffers(self):
        """does editor support multiple open buffers?

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports having multiple buffers open 
        at the same time"""
        return 0

    def bidirectional_selection(self):
        """does editor support selections with cursor at left?

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor allows setting the selection at the
        left end of the selection"""
        return 0

    def app_close_buffer(self, buff_name, save=0):
        """Close a buffer.
        
        **INPUTS**

        STR *buff_name* -- name of buffer to close

        INT *save* -- *-1* -> don't save the buffer
                            *0* -> query user if buffer needs saving
                            *1* -> save without querying user

        **OUTPUTS**
        
        *BOOL* -- true if the editor does close the buffer

        ..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""

        buff = self.find_buff(buff_name)
        if buff == None:
            return 0
        if self.is_bound_to_buffer() == buff_name:
            self.unbind_from_buffer()
        self.open_buffers[buff_name].cleanup()
        del self.open_buffers[buff_name]
        return 1

