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

import WaxEdSim
import SaveSpeech
import GenEdit

import Object
import vc_globals

class WaxEdSimFrameSpeech(WaxEdSim.WaxEdSimFrame):
    """WaxEdSimFrame subclass adding option to save speech files to menu

    **INSTANCE ATTRIBUTES**

    *none*
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
        self.deep_construct(WaxEdSimFrameSpeech, 
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
        WaxEdSim.WaxEdSimFrame.finish_construction(self)
        ID_SAVE_SPEECH_FILES = wxNewId()
        file_menu = self.get_menu_by_name('File')

        config_item = self.make_menu_item(file_menu, ID_SAVE_SPEECH_FILES, 
            "Save s&peech files", help_string = "Save the user's speech files")
        self.insert_item_before_label(config_item, file_menu, 'Exit')
        EVT_MENU(self, ID_SAVE_SPEECH_FILES, self.save_speech_files)

    def save_speech_files(self, event):
        self.parent.save_speech_files()

class WaxEdSimConsoleSpeech(WaxEdSim.WaxEdSimConsole, SaveSpeech.SaveSpeech):
    """WaxEdSimConsole subclass using WaxEdSimFrameSpeech

    **INSTANCE ATTRIBUTES**
    """
    def __init__(self, **args):
        self.deep_construct(WaxEdSimConsoleSpeech, 
                            {}, args)
    
    def confirm_exit(self, ID = None):
        """method called by on_exit which allows the user to save files, 
        etc. or cancel the exit process

        **INPUTS**

        *INT ID* -- ID of the frame sending the event, or None if the
        event doesn't originate from a frame.  (Currently, this
        parameter is ignored).

        **OUTPUTS**

        *BOOL* -- true if the editor is exiting in response to this
        event (unless, e.g., the user has hit cancel in response to a 
        save modified files dialog)
        """
        debug.trace('WaxEdSimConsoleSpeech.confirm_exit',
            'calling GenEditSingle.confirm_exit')
        exiting = GenEdit.GenEditSingle.confirm_exit(self, ID = ID)
        debug.trace('WaxEdSimConsoleSpeech.confirm_exit',
            'GenEditSingle.confirm_exit returned %d' % exiting)
        if not exiting:
            return 0
        if ID:
            frame = self.frames[ID]
        else:
            frame = self.frames[self.frames.keys()[0]]
        debug.trace('WaxEdSimConsoleSpeech.confirm_exit',
            'frame is %s' % repr(frame))
        debug.trace('WaxEdSimConsoleSpeech.confirm_exit',
            'frame is %s' % repr(frame))
        command_space = self.app.access_command_space()
        if not command_space.has_key('save_speech_files') or \
            command_space['save_speech_files'] is None:
            exiting = self.prompt_save_speech_files(frame)
        debug.trace('WaxEdSimConsoleSpeech.confirm_exit',
            'prompt_save_speech_fiels returned %d' % exiting)
        return exiting

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
        return WaxEdSimFrameSpeech(owner = self, app_name = self.app_name,
                ID = wxNewId(), size = self.frame_size, 
                init_buff_name = buff_name, 
                command_space = self.initial_cmd_space)

class WaxEdSimSpeech(WaxEdSim.WaxEdSim):
    """WaxEdSim subclass using WaxEdSimConsoleSpeech

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(WaxEdSimSpeech, 
                            {},
                            args)

    def new_console(self):
        return WaxEdSimConsoleSpeech(app = self, app_name = 'WaxEdSim',
            command_space = self.command_space, curr_dir =
            vc_globals.test_data)

# defaults for vim - otherwise ignore
# vim:sw=4
