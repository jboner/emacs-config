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

"""test version of new GUI interface to the mediator simulation"""


import debug
from wxPython.wx import *

import Object
import GenEdit
import AppStateGenEdit
import vc_globals

from GenEditWX import *

class WaxEdSimPanel(WaxCmdPanel):
    """GUI editor simulator panel

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *wxBitmap green_light, grey_light, dark_grey_light* -- states for
    the microphone button

    *wxBitmapButton mic_button*
    """
    def remove_other_references(self):
        self.green_light = None
        self.grey_light = None
        self.dark_grey_light = None
        WaxCmdPanel.remove_other_references(self)

    def __init__(self, **args):
        self.decl_attrs({'green_light': None,
                         'grey_light': None,
                         'dark_grey_light': None,
                         'mic_button': None
                        })
        self.deep_construct(WaxEdSimPanel, 
                            {
                            }, args)
    
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
        button_line = wxBoxSizer(wxHORIZONTAL)

        self.green_light = wxBitmap("bitmaps/green.bmp", wxBITMAP_TYPE_BMP)
#        self.grey_light = wxBitmap("bitmaps/yellow.bmp", wxBITMAP_TYPE_BMP)
        self.grey_light = wxBitmap("bitmaps/grey.bmp", wxBITMAP_TYPE_BMP)
        self.dark_grey_light = wxBitmap("bitmaps/darkgrey.bmp", wxBITMAP_TYPE_BMP)
        ID_MIC_BUTTON = wxNewId()
        self.mic_button = wxBitmapButton(self, ID_MIC_BUTTON, self.grey_light, 
            wxDefaultPosition, 
            (self.grey_light.GetWidth()+10,self.grey_light.GetHeight()+10))

        ID_MIC_LABEL = wxNewId()
        mic_label = wxStaticText(self, ID_MIC_LABEL, "Microphone: ")

        button_line.Add(mic_label, 0, wxALIGN_CENTER)
        button_line.Add(self.mic_button, 0)
        EVT_BUTTON(self, ID_MIC_BUTTON, self.on_mic_button)

        vbox.Add(button_line, 0)

    def on_mic_button(self, event):
        """handler for button press events from the microphone button
        """
        current = self.command_space['getmic']()
        if current == 'off':
            self.command_space['setmic']('on')
            self.editor_window().SetFocus()
        else:
            self.command_space['setmic']('off')
            self.editor_window().SetFocus()

    def update_mic_button(self, state = None):
        """update the microphone button to reflect the state of the
        microphone

        **INPUTS**

        *STR* state -- new state ('on', 'off', 'sleeping', 'disabled'),
        or None to have update_mic_button check the state

        **OUTPUTS**

        *none*
        """
        if state == None:
            state = self.command_space['getmic']()
        if state == 'on':
            self.mic_button.SetBitmapLabel(self.green_light)
            self.mic_button.Refresh()
        elif state == 'off':
            self.mic_button.SetBitmapLabel(self.grey_light)
            self.mic_button.Refresh()
        else:
            self.mic_button.SetBitmapLabel(self.dark_grey_light)
            self.mic_button.Refresh()

    def access_command_space(self):
        """returns a reference to the command_space namespace being
        used by the panel, allowing access and modifications to that
        namespace after construction of WaxEdSim

        **INPUTS**

        *none*

        **OUTPUTS**

        *{STR:ANY}* -- the namespace dictionary 
        """
        return self.command_space

    def mic_change(self, state):
        """function to receive microphone state change callbacks

        **INPUTS**

        *STR* state -- new state ('on', 'off', 'sleeping', 'disabled')

        **OUTPUTS**

        *none*
        """
        self.update_mic_button(state)

class WaxEdSimFrameMixIn(Object.Object):
    """mix-in class which adds a menus and menu items specific to
    the WaxEdSim console
    **INSTANCE ATTRIBUTES**
    """
    def __init__(self, **args):
        """**NOTE:** some of the tasks which would normally be done 
        in __init__ if this were a subclass of WaxFrameBasic, rather than a 
        mix-in, are instead performed in finish_construction.  
        This is to ensure that those tasks are done after
        WaxFrameBasic's __init__ method is called.
        The common subclass of WaxFrameBasic and this mix-in must 
        call finish_construction after its deep_construct has called 
        WaxFrameBasic's __init__ method.
        """
        self.deep_construct(WaxEdSimFrameMixIn, 
                            {}, args)

    def finish_construction(self):
        """Finish constructing the frame, by adding menu items
        menus
        
        **NOTE:** this task would normally be done 
        in __init__ if this were a subclass of WaxFrameBasic, 
        rather than a mix-in.  Instead it is performed here,
        to ensure that it is done after
        WaxFrameBasic's __init__ method is called.
        The common subclass of WaxFrameBasic and this mix-in must 
        call finish_construction after its deep_construct has called 
        WaxFrameBasic's __init__ method.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        ID_CONF_SCRIPT = wxNewId()
        file_menu = self.get_menu_by_name('File')
#        print file_menu
        config_item = self.make_menu_item(file_menu, ID_CONF_SCRIPT, 
            "Con&fig Script", help_string = "Execute a python configuration script for the environment (ex: a demo file)")
        self.insert_item_before_label(config_item, file_menu, 'Save')
        EVT_MENU(self,ID_CONF_SCRIPT,self.on_execute_file)

    def on_execute_file(self, event):
        self.owner.execute_file()

    def execute_file(self, f_path):
        self.simulate_command("execfile(r'%s')" % f_path)

    def execute_file_dlg(self, init_dir):
        """prompt the user for the filename of a python script to
        execute in the command_space

        **INPUTS**

        *STR* init_dir -- the initial directory for the dialog box
        """
        dlg = wxFileDialog(self, "Execute Script File", init_dir, "",
            "*.*", wxOPEN)
        answer = dlg.ShowModal()
        file_path = None
        if answer == wxID_OK:
            file_path = dlg.GetPath()
        dlg.Destroy()
        return file_path

    def mic_change(self, state):
        """function to receive microphone state change callbacks

        **INPUTS**

        *STR* state -- new state ('on', 'off', 'sleeping', 'disabled')

        **OUTPUTS**

        *none*
        """
        self.pane.mic_change(state)

    def update_mic_button(self, state = None):
        """update the microphone button to reflect the state of the
        microphone

        **INPUTS**

        *STR* state -- new state ('on', 'off', 'sleeping', 'disabled'),
        or None to have update_mic_button check the state

        **OUTPUTS**

        *none*
        """
        self.pane.update_mic_button(state)

class WaxEdSimFrame(WaxEdSimFrameMixIn, WaxCmdFrame):
    """frame containing a WaxEdSimPanel

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(WaxEdSimFrame,
                            {
                            }, args
                           ),
        self.finish_construction()

    def access_command_space(self):
        """returns a reference to the command_space namespace being
        used by the panel, allowing access and modifications to that
        namespace after construction of WaxEdSim

        **INPUTS**

        *none*

        **OUTPUTS**

        *{STR:ANY}* -- the namespace dictionary 
        """
        return self.pane.access_command_space()

    def add_pane(self, ID):
        """create the actual WaxPanel for the frame

        **INPUTS**

        *wxWindowId ID* -- the ID of the panel
        """
        return WaxEdSimPanel(parent = self, ID = ID, 
            command_space = self.command_space)

class SimConsole(GenEdit.ActivateEventMixIn, GenEdit.GenEditSingle):
    """mix-in for concrete simulator console version of 
    GenEditSingle (GenEditFrames)

    **INSTANCE ATTRIBUTES**

    *WaxEdSim, wxApp app* -- the application which owns this object

    *STR app_name* -- the name of the application
    """
    def __init__(self, app, **args):
        self.deep_construct(SimConsole, 
                            {'app': app
                            }, args)
        self.name_parent('app')
    
    def execute_file(self):
        frame = self.active_frame()
        init_dir = self.curr_dir
        f_path = frame.execute_file_dlg(init_dir)
        if f_path:
            frame.execute_file(f_path)
    
    def access_command_space(self):
        """returns a reference to the command_space namespace being
        used by the panel, allowing access and modifications to that
        namespace after construction of WaxEdSim

        **INPUTS**

        *none*

        **OUTPUTS**

        *{STR:ANY}* -- the namespace dictionary 
        """
        frame = self.active_frame()
        return frame.access_command_space()

    def mic_change(self, state):
        """function to receive microphone state change callbacks

        **INPUTS**

        *STR* state -- new state ('on', 'off', 'sleeping', 'disabled')

        **OUTPUTS**

        *none*
        """
        for frame in self.frames.values():
            frame.mic_change(state)

    def update_mic_button(self, state = None):
        """update the microphone button to reflect the state of the
        microphone

        **INPUTS**

        *STR* state -- new state ('on', 'off', 'sleeping', 'disabled'),
        or None to have update_mic_button check the state

        **OUTPUTS**

        *none*
        """
        for frame in self.frames.values():
            frame.update_mic_button(state)
        
class WaxEdSimConsole(SimConsole):
    """concrete simulator console version of 
    GenEditSingle (GenEditFrames)

    **INSTANCE ATTRIBUTES**

    *wxSize or (INT, INT) size* -- default size for frames

    *{STR: ANY}* initial_cmd_space -- initial name space for user commands
    entered at the command line 

    """
    def __init__(self, frame_size = None, 
        command_space = None, **args):
        self.decl_attrs({'frame_size': frame_size,
                         'initial_cmd_space': command_space})
        if frame_size == None:
            self.frame_size = (1000, 600)
        self.deep_construct(WaxEdSimConsole, 
                            {}, args)

    def initial_frame(self):
        """returns a reference to the main (initial) frame of the wxMediator
        application

        **INPUTS**

        *none*

        **OUTPUTS**

        *wxFrame* -- the main wxFrame
        """
# based on GenEditSingle, so only one frame
        return self.frames[self.only_ID]

    def new_frame(self, buff_name):
        """creates a new frame of the appropriate concrete class
        open buffer and new window callbacks to the AppState interface

        **NOTE:** when adding a new frame with a buffer, you should call
        new_buffer first, followed by add_frame

        **INPUTS**

        *STR buff_name* -- the name of the initial buffer for the frame

        **OUTPUTS**

        *INT* -- ID of the new frame, or None if the frame was not 
        added successfully
        """
        return WaxEdSimFrame(owner = self, app_name = self.app_name,
                ID = wxNewId(), size = self.frame_size, 
                init_buff_name = buff_name, 
                command_space = self.initial_cmd_space)

class WaxEdSimBase(Object.OwnerObject):
    """base class for the wxApp application class for the WaxEdSim console

    **INSTANCE ATTRIBUTES**

    *WaxEdSimConsole* console -- the GUI console manager

    *AppStateGenEdit editor* -- the AppState wrapper interface to 
    the console

    *{STR:ANY}* command_space -- a namespace (dictionary) in which to
    execute commands from the command line
    """
    def __init__(self, command_space = None, **args):
        """
        **INPUTS**

        *{STR:ANY}* command_space -- a namespace (dictionary) in which to
        execute commands from the command line
        """
        self.decl_attrs({
                         'console': None,
                         'editor': None
                        }
                       )

        self.deep_construct(WaxEdSimBase, 
                            {
                             'command_space': command_space
                            },
                            args
                           )
        self.add_owned('console')
        self.add_owned('editor')

    def access_command_space(self):
        """returns a reference to the command_space namespace being
        used by the panel, allowing access and modifications to that
        namespace after construction of WaxEdSim

        **INPUTS**

        *none*

        **OUTPUTS**

        *{STR:ANY}* -- the namespace dictionary 
        """
        return self.wax_console.access_command_space()

    def mic_change(self, state):
        """function to receive microphone state change callbacks

        **INPUTS**

        *STR* state -- new state ('on', 'off', 'sleeping', 'disabled')

        **OUTPUTS**

        *none*
        """
        self.wax_console.mic_change(state)


    def update_mic_button(self, state = None):
        """update the microphone button to reflect the state of the
        microphone

        **INPUTS**

        *STR* state -- new state ('on', 'off', 'sleeping', 'disabled'),
        or None to have update_mic_button check the state

        **OUTPUTS**

        *none*
        """
        self.wax_console.update_mic_button(state)

class WaxEdSim(wxApp, WaxEdSimBase):
    """application class for the WaxEdSim console

    **INSTANCE ATTRIBUTES**
    """
    def __init__(self, **args):
        self.decl_attrs({'wax_console': None})
        self.deep_construct(WaxEdSim, 
                            {
                            },
                            args,
                            exclude_bases = {wxApp:1}
                           )
        wxApp.__init__(self, 1, 'simcrash')
#        wxApp.__init__(self, 0)

    def OnInit(self):
        self.wax_console = self.new_console()
        frame = self.wax_console.active_frame()
        self.SetTopWindow(frame)
        self.editor = AppStateGenEdit.AppStateGenEdit(self.wax_console)
        return true

    def new_console(self):
        return WaxEdSimConsole(app = self, app_name = 'WaxEdSim',
            command_space = self.command_space, curr_dir =
            vc_globals.test_data)

    def run(self):
        """starts the message loop.  Note: this function does not
        return until the GUI exits.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.update_mic_button()
        self.MainLoop()
        self.cleanup()

def no_mic_getmic():
    return 'off'
def no_mic_setmic(state):
    pass

def run():
    """run for testing without mediator or speech engine"""
    command_space = {}
    command_space['quit_flag'] = 0
    command_space['getmic'] = no_mic_getmic
    command_space['setmic'] = no_mic_setmic
    command_space['setmic'] = no_mic_setmic
    app=WaxEdSim(command_space = command_space)
    command_space['changed'] = 'too late'
    actual_space = app.access_command_space()
    actual_space['late_change'] = 'should be possible'
    app.run()
#    app.MainLoop()

if __name__ =='__main__':
    run()
  
# defaults for vim - otherwise ignore
# vim:sw=4
