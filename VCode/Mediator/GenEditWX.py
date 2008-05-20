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
# (C)2000, David C. Fox
#
##############################################################################

"""test version of new WaxEdit client editor"""


import copy
import debug
import traceback
import sys
import code # for interpreting Python code at the command line
import Object 
#import wxEditor
from wxPython.wx import *

try:
    dummy_var = wxCHANGE_DIR
    del dummy_var
except NameError:
    wxCHANGE_DIR = 0

import TextBufferWX
import GenEdit
import wxCmdPrompt
import wxAutoSplitterWindow
from wxFrameMenuMixIn import wxFrameMenuMixIn

class SingleBufferWindow:
    """abstract base class for an editor window main panel 
    containing a single text buffer 

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def editor_window(self):
        """returns a reference to the editor window

        **INPUTS**

        *none*

        **OUTPUTS**

        *wxTextCtrl* -- the editor window
        """
        debug.virtual('SingleBufferWindow.editor_window')

    def editor_buffer(self):
        """returns a reference to the TextBufferWX embedded in the GUI

        **INPUTS**

        *none*

        **OUTPUT**

        *TextBufferWX* -- the TextBufferWX
        """
        debug.virtual('SingleBufferWindow.editor_buffer')

    def editor_has_focus(self):
        """indicates whether the editor window has the focus

        **INPUTS**

        *none*

        **OUTPUTS**
        *BOOL* -- true if editor window has the focus
        """
        return 1
        current = wxWindow_FindFocus()
        if current and current.GetId() == self.editor_window().GetId():
            return 1
        return 0

    def current_font(self):
        """find the current font for the text buffers in this window

        **INPUTS**

        *none*

        **OUTPUTS**

        *wxFont* -- the current font
        """
        return self.editor_window().GetFont()

    def set_font(self, font):
        """sets the current font for the text buffers in this window

        **INPUTS**

        *wxFont font* -- the desired font

        **OUTPUTS**

        *none*
        """
        self.editor_window().SetFont(font)

class EditorBuilder(Object.Object):
    """mix-in class for creating the editor buffer window and its
    TextBufferWX wrapper
    """
    def __init__( self, **args):
        self.deep_construct(EditorBuilder, {}, args)

    def build_editor_buffer(self, parent, use_rich = 0):
        """builds an editor window
        
        **INPUTS**
        
        *wxWindow parent* -- the parent of the editor window

        *BOOL use_rich* -- flag indicating whether we should use a
        standard text control or a rich edit control on Windows

        **OUTPUTS**

        *(wxTextCtrl, TextBufferWX)* -- the editor window and its
        TextBufferWX wrapper
        """
        debug.virtual('EditorBuilder.build_editor_buffer')

class EditorBuilderBasic(EditorBuilder):
    """mix-in class for creating the editor buffer window and its
    TextBufferWX wrapper
    """
    def __init__(self, **args):
        self.deep_construct(EditorBuilderBasic, {}, args)

    def build_editor_buffer(self, parent, use_rich = 0):
        """builds an editor window
        
        **INPUTS**
        
        *wxWindow parent* -- the parent of the editor window

        *BOOL use_rich* -- flag indicating whether we should use a
        standard text control or a rich edit control on Windows

        **OUTPUTS**

        *(wxTextCtrl, TextBufferWX)* -- the editor window and its
        TextBufferWX wrapper
        """
        flags = wxTE_MULTILINE | wxTE_NOHIDESEL  
        cr_bug = 0
        if sys.platform == 'win32':
            cr_bug = 1
            if use_rich:
# allows text longer than 64K
                flags = flags | wxTE_RICH
                cr_bug = 0
# rich text uses \r only for new lines, so offsets into internal and 
# external buffers are the same

        ID_EDITOR = wxNewId()
        editor = wxTextCtrl(parent, ID_EDITOR, "", wxDefaultPosition,
            wxDefaultSize, flags)
#            self.GetClientSize(), wxTE_MULTILINE)
#            wxDefaultSize, wxTE_MULTILINE | wxTE_NOHIDESEL)
        wax_text_buffer = \
            TextBufferWX.TextBufferWX(editor, carriage_return_bug = cr_bug)
        return editor, wax_text_buffer
    
class WaxPanel(wxPanel, EditorBuilderBasic, SingleBufferWindow,
    Object.OwnerObject):
    """abstract base class for a main panel containing 
    a single text buffer 

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *TextBufferWX* wax_text_buffer -- editor interface with change
    specification, so that we can keep track of changes to the editor
    buffer.

    *wxTextControl* editor -- underlying text control for editor window

    *WaxFrame, wxWindow parent* -- the parent frame window to the panel

    *BOOL* closing -- true if panel is closing (used to ensure that
    event handlers don't continue to call other methods when the panel
    may not be in a sane state)
    """
    def remove_other_references(self):
        """additional cleanup to ensure that this object's references to
        its owned objects are the last remaining references

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
# subclasses must call their parent class's remove_other_references
# function, after performing their own duties
        self.closing = 1
        self.wax_text_buffer = None
        self.editor = None
        Object.OwnerObject.remove_other_references(self)

    def initial_show(self):
        """**NOTE:** the parent frame must call this method when it 
        is initially shown.
        """
        pass
    
    def __init__(self, parent, ID, use_rich = 0, **args):
        """
        **INPUTS**

        *WaxFrame, wxWindow parent* -- the parent frame window to the panel

        *INT ID* -- the ID of the panel

        *BOOL use_rich* -- flag indicating whether we should use a
        standard text control or a rich edit control on Windows

        """
        self.deep_construct(WaxPanel,
                            {'wax_text_buffer': None,
                             'parent': parent,
                             'editor': None,
                             'closing': 0
                            },
                            args,
                            exclude_bases = {wxPanel: 1, SingleBufferWindow: 1}
                           )
        wxPanel.__init__(self, parent, ID, wxDefaultPosition, 
            wxDefaultSize)
        self.name_parent('parent')

        vbox = wxBoxSizer(wxVERTICAL)
        self.prepend(vbox)

        editor, buffer = self.add_editor_buffer(vbox, use_rich = use_rich)

        self.editor = editor
        self.wax_text_buffer = buffer

        self.append(vbox)


        self.SetAutoLayout(1)
        self.SetSizer(vbox)
        vbox.Fit(self)
        vbox.SetSizeHints(self)

    def add_editor_buffer(self, vbox, use_rich = 0):
        """builds an editor window (and, optionally, other associated
        controls) and adds it (them) to the panel and sizer
        
        **INPUTS**

        **Note:** the additional controls should either be children
        of the panel (self) or children of children, etc.  If any 
        cleanup is necessary (apart from the automatic destruction of 
        the controls when the panel exits), it 
        must be added to the subclass's remove_other_references method.
        
        *wxBoxSizer vbox* -- the vertical box sizer to which the
        controls should be added.

        *BOOL use_rich* -- flag indicating whether we should use a
        standard text control or a rich edit control on Windows

        **OUTPUTS**

        *(wxTextCtrl, TextBufferWX)* -- the editor window and its
        TextBufferWX wrapper
        """
        debug.virtual('WaxPanel.add_editor_buffer')

    def prepend(self, vbox):
        """ allows subclasses to add additional controls to the panel
        above the editor window

        **Note:** the additional controls should have the panel (self)
        as a parent.  If any cleanup is necessary (apart from the
        automatic destruction of the controls when the panel exits), it 
        must be added to the subclass's remove_other_references method.

        **INPUTS**

        *wxBoxSizer vbox* -- the vertical box sizer to which the
        controls should be added.

        **OUTPUTS**

        *none*
        """
        pass
    
    def append(self, vbox):
        """ allows subclasses to add additional controls to the panel
        below the editor window

        **Note:** the additional controls should have the panel (self)
        as a parent.  If any cleanup is necessary (apart from the
        automatic destruction of the controls when the panel exits), it 
        must be added to the subclass's remove_other_references method.

        **INPUTS**

        *wxBoxSizer vbox* -- the vertical box sizer to which the
        controls should be added.

        **OUTPUTS**

        *none*
        """
        pass

    def editor_window(self):
        """returns a reference to the editor window

        **INPUTS**

        *none*

        **OUTPUTS**

        *wxTextCtrl* -- the editor window
        """
        return self.editor

    def editor_buffer(self):
        """returns a reference to the TextBufferWX embedded in the GUI

        **INPUTS**

        *none*

        **OUTPUT**

        *TextBufferWX* -- the TextBufferWX
        """
        return self.wax_text_buffer

class SimpleWaxPanel(WaxPanel):
    """concrete implementation for a main panel containing only
    a single text buffer 

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(SimpleWaxPanel,
                            {
                            },
                            args
                           )

    def add_editor_buffer(self, vbox, use_rich = 0):
        """builds an editor window (and, optionally, other associated
        controls) and adds it (them) to the panel and sizer
        
        **INPUTS**

        **Note:** the additional controls should either be children
        of the panel (self) or children of children, etc.  If any 
        cleanup is necessary (apart from the automatic destruction of 
        the controls when the panel exits), it 
        must be added to the subclass's remove_other_references method.
        
        *wxBoxSizer vbox* -- the vertical box sizer to which the
        controls should be added.

        *BOOL use_rich* -- flag indicating whether we should use a
        standard text control or a rich edit control on Windows

        **OUTPUTS**

        *(wxTextCtrl, TextBufferWX)* -- the editor window and its
        TextBufferWX wrapper
        """
        editor, buffer = \
            self.build_editor_buffer(parent = self, use_rich = use_rich)

# because we put the editor in a panel, we need a sizer
        vbox.Add(editor, 1, wxGROW)
        return editor, buffer

class WaxCmdPanel(WaxPanel):
    """concrete implementation for a main panel containing a command
    line and log window in addition to a single text buffer 

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *wxAutoSplitterWindow* top_and_bottom -- parent window of editor and
    log windows, with adjustable sash

    *STR* prompt_text -- text of prompt, as printed in log window

    *{STR: ANY}* command_space -- local name space for user commands
    entered at the command line 

    *wxCmdLog* command_log -- handler to prove editable command-line 
    with history, and log

    *wxTextCtrl* command_line -- the command-line GUI control itself

    *InteractiveInterpreter* command_line_interp -- from standard module
    code

    *wxTextControl* log -- text control for log window to display
    output, error messages, and command history
    """
    def __init__(self, command_space = None, prompt_text = 'Command> ', **args):
# have to pre-declare these attributes, because add_editor_buffer, which
# sets their values, is executed from the WaxPanel constructor before
# deep_construct would get around to declaring them
        self.decl_attrs(
                        {
                         'top_and_bottom': None, 
                         'prompt_text': prompt_text,
                         'command_space': copy.copy(command_space),
                         'command_log': None, 
                         'command_line': None, 
                         'log': None, 
                         'command_line_interp': None
                        }
                       )
        self.deep_construct(WaxCmdPanel,
                            {},
                            args
                           )


    def add_editor_buffer(self, vbox, use_rich = 0):
        """builds an editor window (and, optionally, other associated
        controls) and adds it (them) to the panel and sizer
        
        **INPUTS**

        **Note:** the additional controls should either be children
        of the panel (self) or children of children, etc.  If any 
        cleanup is necessary (apart from the automatic destruction of 
        the controls when the panel exits), it 
        must be added to the subclass's remove_other_references method.
        
        *wxBoxSizer vbox* -- the vertical box sizer to which the
        controls should be added.

        *BOOL use_rich* -- flag indicating whether we should use a
        standard text control or a rich edit control on Windows

        **OUTPUTS**

        *(wxTextCtrl, TextBufferWX)* -- the editor window and its
        TextBufferWX wrapper
        """
        ID_SPLITTER = wxNewId()
        top_and_bottom = wxAutoSplitterWindow.wxFixedFocusSplitter(self,
            ID_SPLITTER, 1)
        top_and_bottom.SetMinimumPaneSize(30)

#        print top_and_bottom

        editor, buffer = \
            self.build_editor_buffer(parent = top_and_bottom, 
            use_rich = use_rich)

        flags = wxTE_MULTILINE | wxTE_READONLY
        if use_rich:
# allows text longer than 64K
            flags = flags | wxTE_RICH

        ID_LOG = wxNewId()
        log = wxTextCtrl(top_and_bottom, ID_LOG, "", wxDefaultPosition,
            wxDefaultSize, flags)

        self.log = log
        self.command_log = wxCmdPrompt.wxCmdLog(log, prompt = self.prompt_text)

        self.prompt_line = wxBoxSizer(wxHORIZONTAL)

        ID_PROMPT = wxNewId()
        self.prompt = wxStaticText(self, ID_PROMPT, "Co&mmand")

        ID_COMMAND_LINE = wxNewId()
        command_line = wxTextCtrl(self, ID_COMMAND_LINE, 
            "", wxDefaultPosition, wxDefaultSize,
            style =wxTE_PROCESS_ENTER)
        self.command_line = command_line

        self.prompt_line.Add(self.prompt, 0, wxALL, 4)
        self.prompt_line.Add(self.command_line, 1, wxALL, 4)

#        EVT_SET_FOCUS(self, self.on_focus)
#        EVT_SET_FOCUS(self.prompt, self.p_focus)


        if self.command_space == None:
            self.command_space = {}

# provide extra access for testing 
        self.command_space['the_pane'] = self
        self.command_prompt = wxCmdPrompt.wxCmdPromptWithHistory(command_line,
            command_callback = self.on_command_enter)

        self.command_line_interp = \
            code.InteractiveInterpreter(self.command_space)


# because we put the editor in a panel, we need a sizer
        vbox.Add(top_and_bottom, 1, wxEXPAND | wxALL, 4)
        vbox.Add(self.prompt_line, 0, wxEXPAND | wxALL, 4)

        self.top_and_bottom = top_and_bottom
#        print self.top_and_bottom

        return editor, buffer

    def initial_show(self):
        """create editor and log windows.  This is done here, rather
        than in __init__ because this is the first time that the actual
        size of the parent splitter window, top_and_bottom, is known
        """
        self.top_and_bottom.SplitHorizontally(self.editor, self.log, 0)
    
    def simulate_command(self, command):
        """simulate a command being entered on the command line

        **INPUTS**

        *STR* command -- the command to simulate

        **OUTPUTS**

        *none*
        """
        self.on_command_enter(self.command_line, command)
        
    def on_command_enter(self, command_line, command):
        """handler for Enter pressed in the command line

        **INPUTS**

        *wxTextCtrl* command_line -- the command line

        *STR* command -- the command which was entered

        **OUTPUTS**

        *none*
        """
        self.command_log.log_command(command)
        stdout = sys.stdout
        stderr = sys.stderr
        # capture standard output and standard error from exec and redirect them
        # to the command log
        self.command_space['quit_flag'] = 0
        try:
            sys.stdout = self.command_log
            sys.stderr = self.command_log
            self.editor.SetFocus()
            try:
                if self.command_line_interp.runsource(command):
                    sys.stderr.write('Error: incomplete input\n')
#                exec command \
#                    in sys.modules[self.__class__.__module__].__dict__, \
#                    self.command_space
                #  exec command in self.command_space
            except Exception, err:
                traceback.print_exc()

        finally:
            # make sure to restore standard output and standard error,
            # so that errors in the GUI don't go unreported if the GUI
            # crashes
            self.command_line.SetFocus()
            sys.stdout = stdout
            sys.stderr = stderr

#        print self.command_space['quit_flag']
        if self.command_space['quit_flag']:
            self.parent.quit_now(None)


class WaxFrameBase(wxFrame, GenEdit.GenEditFrameActivateEvent,
    wxFrameMenuMixIn):
    """partially concrete base class containing GUI elements and
    behaviors common to many subclasses.  Note: this class does not
    fill the frame with anything.

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *wxWindow parent* -- the parent window, if any (usually not for
    single-frame applications)

    *BOOL* closing -- true if frame is closing (used to ensure that
    event handlers don't continue to call other methods when the frame
    may not be in a sane state)

    *{STR: ANY}* command_space -- local name space for user commands
    entered at the command line (ignored by subclasses without command
    lines)

    *wxWindow most_recent_focus* -- the control which had the focus when
    the application was last deactivated

    others -- the various menus
    """
    def remove_other_references(self):
        """additional cleanup to ensure that this object's references to
        its owned objects are the last remaining references

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
# subclasses must call their parent class's remove_other_references
# function, after performing their own duties

        self.closing = 1
        self.most_recent_focus = None
        GenEdit.GenEditFrameActivateEvent.remove_other_references(self)

    def __init__(self, ID, size, parent = None, **args):
        """constructor for the base WaxEditFrame class

        **INPUTS**

        *wxWindowID ID* -- the wxWindows ID for the frame window

        *wxSize or (INT, INT) size* -- initial size for the frame

        *wxWindow parent* -- the parent window, if any (usually not for
        single-frame applications)
        """

        self.deep_construct( WaxFrameBase,
                            {'closing': 0,
                             'ID': ID
                            }, args,
                            exclude_bases = {wxFrame: 1},
                            enforce_value = {'ID': ID}
                           )
        wxFrame.__init__(self, parent, self.ID, self.app_name, 
            wxDefaultPosition, size)

        file_menu=wxMenu()
        ID_OPEN_FILE = wxNewId()
        ID_SAVE_FILE = wxNewId()
        ID_SAVE_AS = wxNewId()
        ID_EXIT = wxNewId()
        ID_CLOSE_MENU = wxNewId()
#        print ID_OPEN_FILE
        file_menu.Append(ID_OPEN_FILE,"&Open...","Open a file")
        file_menu.Append(ID_SAVE_FILE,"&Save","Save current file")
        file_menu.Append(ID_SAVE_AS,"Save &As...","Save current file")        
        file_menu.Append(ID_CLOSE_MENU,"&Close","Close window")
        file_menu.Append(ID_EXIT,"E&xit","Terminate")

        ID_CHOOSE_FONT = wxNewId()
        format_menu = wxMenu()
        format_menu.Append(ID_CHOOSE_FONT, "&Font...")        

        edit_menu = wxMenu()

        menuBar=wxMenuBar()
        EVT_CLOSE(self, self.on_close)        
        menuBar.Append(file_menu,"&File");
        menuBar.Append(edit_menu,"&Edit");
        menuBar.Append(format_menu,"F&ormat");        
#        menuBar.Append(window_menu, "&Window");

        self.CreateStatusBar()

        self.SetMenuBar(menuBar)
        EVT_MENU(self, ID_EXIT, self.quit_now)
        EVT_MENU(self, ID_CLOSE_MENU, self.close_now)
        EVT_MENU(self, ID_OPEN_FILE, self.on_open_file)
        EVT_MENU(self, ID_SAVE_FILE,self.on_save)
        EVT_MENU(self, ID_SAVE_AS,self.on_save_as)        
        EVT_MENU(self, ID_CHOOSE_FONT, self.choose_font)
        EVT_ACTIVATE(self, self.OnActivate) 
        self.most_recent_focus = None

    def set_status_text(self, text):
        self.SetStatusText(text)

    def show(self, initial = 0):
        """show the window corresponding to this frame

        **INPUTS**

        *BOOL* initial -- is this the initial time the frame is shown?

        **OUTPUTS**

        *none*
        """
        self.Show(1)
        self.update_title()
#        print 'showing'
        if initial:
            self.initial_show()

    def initial_show(self):
        """**NOTE:** the application must call this method when the
        frame is initially shown.
        """
        pass

    def OnActivate(self, event):
        if self.closing:
            return
        current = wxWindow_FindFocus()
        if event.GetActive():
# window is being activated

# if activated by mouse, some control of this application may already
# be focused, and we don't want to override that, but otherwise
# we want to set the focus back to the control which had it last
            if self.most_recent_focus and not current:
                self.most_recent_focus.SetFocus()
            self.on_activate(1)
        else:
            self.most_recent_focus = current
            self.on_activate(0)

    def full_title(self):
        """constructs the full title for the frame from the app_name,
        instance_string, if any, and buffer name

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the title string
        """
        buff_name = self.frame_active_buffer_name()
        title = self.app_name + ' - '
        if self.instance_string:
            title = title + '%s - ' % self.instance_string
        title = title + buff_name
        return title

    def update_title(self):
        """update the window title of the frame reflect a new buffer name 
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.SetTitle(self.full_title())
    
    def close_window(self):
        """close the window corresponding to this frame

        **NOTE:** The owner must call the frame's cleanup method before
        calling this method to close the actual GUI frame

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if not self.closing:
# this should never happen, but if it does, report the error and make
# the best of it
            msg = 'frame received close_window without prior cleanup\n'
            debug.critical_warning(msg)
# this is diagnostic information associated with the critical warning, 
# not a trace, so it should always be printed
#            debug.print_call_stack()
            self.cleanup()
        debug.trace('WaxFrameBase.close_window', 'calling self.Close')
        self.Close()

    def close_now(self, event):
        """handler for Close menu item
        """
        debug.trace('WaxFrameBase.close_now', 'calling self.Close')
        self.Close()

    def quit_now(self, event):
# owner will be responsible for prompting for the user to save files,
# and calling cleanup for this frame (and all others)
#        print 'quit_now'
        debug.trace('WaxFrameBase.quit_now', 'calling owner.on_exit')
        self.owner.on_exit(self.frame_ID)
    
    def on_close(self, event):
# after the owner has cleaned up the frame (on exit), go ahead and close
#        print 'on_close'
        debug.trace('WaxFrameBase.on_close', 
            'self.closing = %d' % self.closing)
        if self.closing:
#            print 'closing'
            event.Skip()
            return

# otherwise, notify the owner, which will be responsible for 
# prompting for the user to save files.
        proceed = self.owner.on_frame_close(self.frame_ID)
#        print 'proceed = ', proceed
# Unless the user cancels closing the frame, the owner will
# call cleanup for this frame, so it will be safe to close the frame
        if proceed:
            event.Skip()
    
    def open_file_dialog(self, init_dir):
        file_path = None
        dlg = wxFileDialog(self, "Edit File", init_dir, "", "*.*",
            wxOPEN | wxCHANGE_DIR)
        answer = dlg.ShowModal()
        if answer == wxID_OK:
            file_path = dlg.GetPath()
        dlg.Destroy()
        return file_path

    def on_open_file(self, event):
# this is somewhat roundabout, but it allows us to get the proper
# initial directory for the Save As dialog box, and ensure that AppState
# gets an open buffer callback
#        print 'on_open'
        self.owner.open_file(user_initiated = 1)

    def save_as_dialog(self, buff_name, init_dir):
        """prompts for a filename under which to save the file, and
        confirms overwriting

        **INPUTS**

        *STR buff_name* -- the name of the buffer (used to find the
        corresponding frame over which to pop up the Save As dialog)

        *STR init_dir* -- the path of the initial directory for the Save
        As dialog

        **OUTPUTS**

        *STR* -- the specified path, or None if the user cancelled 
        """
        dlg = wxFileDialog(self, "Save File", init_dir,  "", "*.*",
            wxSAVE | wxCHANGE_DIR | wxOVERWRITE_PROMPT)
        file_path = None
        answer = dlg.ShowModal()
        if answer == wxID_OK:
            file_path = dlg.GetPath()
        dlg.Destroy()
        return file_path

    def on_save_as(self, event):
#        print 'on_save_as'
        buff_name = self.frame_active_buffer_name()
# this is somewhat roundabout, but it allows us to get the proper
# initial directory for the Save As dialog box, and ensure that AppState
# gets a rename buffer callback
        self.owner.save_file(buff_name, ask_for_new_name = 1,
            user_initiated = 1)

    def on_save(self, event):
#        print 'on_save'
        buff_name = self.frame_active_buffer_name()
        self.owner.save_file(buff_name, user_initiated = 1)

   
    def prompt_to_save(self, buff_name):
        """prompts the user to save the current buffer before closing it, 
        or cancel.  Note: prompt_to_save should save if the user so
        indicates, and update the entry in self.filenames corresponding
        to the buffer, but should not close the buffer, because
        open_file could still fail.

        **INPUTS**

        *STR buff_name* -- the name of the buffer
        
        **OUTPUTS**

        *BOOL* -- true if the user saved or told WaxEdit to proceed
        without saving, false if the user asked for the action causing
        the buffer closing to be cancelled.
        """
        answer = wxMessageBox("Save changes to document %s?" % buff_name, 
                "Save Changes", 
                wxICON_EXCLAMATION | wxYES_NO | wxCANCEL | wxYES_DEFAULT, 
                self)
        if answer == wxCANCEL:
            return 0
        if answer == wxYES:
            new_buff_name = self.owner.save_file(buff_name, rename_buff = 0)
            if not new_buff_name:
                return 0
        return 1
 
    def current_font(self):
        """find the current font for the text buffers in this window

        **INPUTS**

        *none*

        **OUTPUTS**

        *wxFont* -- the current font
        """
        debug.virtual('WaxFrameBase.current_font')

    def set_font(self, font):
        """sets the current font for the text buffers in this window

        **INPUTS**

        *wxFont font* -- the desired font

        **OUTPUTS**

        *none*
        """
        debug.virtual('WaxFrameBase.set_font')

    def choose_font(self, event):
        current_font = self.current_font()
        current_font_data = wxFontData()
        current_font_data.SetInitialFont(current_font)
        dlg = wxFontDialog(self, current_font_data)
# the line below passed the return value (None?) of SetInitialFont to the
# dialog
#        dlg = wxFontDialog(self, wxFontData().SetInitialFont(current_font))
        dlg.ShowModal()
        chosen_font = dlg.GetFontData().GetChosenFont()
        if chosen_font:
            self.set_font(chosen_font)
        dlg.Destroy()            

class WaxFrame(WaxFrameBase):
    """partially concrete class for frame containing a WaxPanel

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *WaxPanel* pane -- panel containing the controls

    *STR* curr_buffer_name -- name of the current (and only) buffer

    others -- the various menus
    """
    def __init__(self, init_buff_name = "", **args):
        self.deep_construct( WaxFrame,
                            {'pane': None,
                             'curr_buffer_name': init_buff_name
                            }, args
                           ),
        ID_PANE = wxNewId()
        self.pane = self.add_pane(ID = ID_PANE)
        self.add_owned('pane')

    def add_pane(self, ID):
        """create the actual WaxPanel for the frame

        **INPUTS**

        *wxWindowId ID* -- the ID of the panel
        """
        debug.virtual('WaxPane.add_pane')

    def initial_show(self):
        """**NOTE:** the application must call this method when the
        frame is initially shown.
        """
        self.pane.initial_show()

    def frame_active_buffer_name(self):
        """Returns the name of the buffer currently active in this
        frame.

        **INPUTS**

        *none* 
        
        **OUTPUTS**

        *STR* -- buffer name of current buffer, or None if there is none
        """
        return self.curr_buffer_name
    
    def open_buffers(self):
        """retrieve a list of the names of open buffers associated with
        contained by this frame.

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of the names of open buffers
        """
        return [self.curr_buffer_name]

    def rename_buffer(self, buff_name, new_buff_name):
        """notifies the frame that one of its buffers has been renamed

        **INPUTS**

        *STR buff_name* -- the old name of the new buffer

        *STR new_buff_name* -- the new name of the new buffer

        **OUTPUTS**

        *BOOL* -- false if the old buff_name was unknown, or the new
        name is already present
        """
        if self.curr_buffer_name == buff_name:
            self.curr_buffer_name = new_buff_name
            return 1
        else:
            return 0
    
    def switch_to_buffer(self, buff_name):
        """Puts this frame in the foreground (if it isn't already), and
        changes the active buffer to buff_name

        **INPUTS**
        
        STR *buff_name* -- Name of the buffer to switch to.
       
        **OUTPUTS**
        
        *BOOL* -- true if buff_name exists and the external application
        successfully switches to it
        """
        if buff_name == self.curr_buffer_name:
            self.SetFocus()
            return 1
        return 0

    def delete_buffer(self, buff_name):
        """delete a buffer 

        **INPUTS**

        STR *buff_name* -- name of buffer to remove

        **OUTPUTS**

        *none*
        """
# for frames with only one buffer, this is a no-op
        pass
 
    def remove_buffer(self, buff_name):
        """remove a buffer from the list of belong to this frame.  

        **Note:** this method only removes the buffer from GenEditFrame's
        records, it does not destroy the underlying GUI buffer or the
        window containing it.

        **INPUTS**

        STR *buff_name* -- name of buffer to remove

        **OUTPUTS**

        *none*
        """
# for frames with only one buffer, this is a no-op
        pass

    def current_font(self):
        """find the current font for the text buffers in this window

        **INPUTS**

        *none*

        **OUTPUTS**

        *wxFont* -- the current font
        """
        return self.pane.current_font()

    def set_font(self, font):
        """sets the current font for the text buffers in this window

        **INPUTS**

        *wxFont font* -- the desired font

        **OUTPUTS**

        *none*
        """
        self.pane.set_font(font)
    
    def editor_buffer(self, buff_name):
        """returns a reference to the TextBufferWX embedded in the GUI

        **INPUTS**

        *none*

        **OUTPUT**

        *TextBufferWX* -- the TextBufferWX
        """
        if buff_name == self.frame_active_buffer_name():
            return self.pane.editor_buffer()
        else:
            return None

    def editor_has_focus(self):
        """indicates whether the editor window has the focus

        **INPUTS**

        *none*

        **OUTPUTS**
        *BOOL* -- true if editor window has the focus
        """
        return self.pane.editor_has_focus()

# defaults for vim - otherwise ignore
# vim:sw=4

class SimpleWaxFrame(WaxFrame):
    """frame containing a SimpleWaxPanel

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct( SimpleWaxFrame,
                            {
                            }, args
                           ),

    def add_pane(self, ID):
        """create the actual WaxPanel for the frame

        **INPUTS**

        *wxWindowId ID* -- the ID of the panel
        """
        return SimpleWaxPanel(parent = self, ID = ID)

class WaxCmdFrame(WaxFrame):
    """frame containing a WaxCmdPanel

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *{STR: ANY}* command_space -- local name space for user commands
    entered at the command line (ignored by subclasses without command
    lines)

    others -- the various menus
    """
    def __init__(self, command_space = None, **args):
        self.decl_attrs(
                        {
                         'command_space': command_space,
                        }
                       )
        self.deep_construct( WaxCmdFrame,
                            {
                            }, args
                           ),

    def add_pane(self, ID):
        """create the actual WaxPanel for the frame

        **INPUTS**

        *wxWindowId ID* -- the ID of the panel
        """
        return WaxCmdPanel(parent = self, ID = ID, 
            command_space = self.command_space)

    def simulate_command(self, command):
        """simulate a command being entered on the command line

        **INPUTS**

        *STR* command -- the command to simulate

        **OUTPUTS**

        *none*
        """
        self.pane.simulate_command(command)
        

# defaults for vim - otherwise ignore
# vim:sw=4

