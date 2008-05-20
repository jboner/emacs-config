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
# (C)2002, National Research Council of Canada
#
##############################################################################

"""A NewMediatorObject-based VoiceCode server that uses TCP/IP based 
messaging protocol to communicate with external editors.
"""

import vc_globals

import debug
import Object
import sr_interface, util

from wxPython.wx import *

class SaveSpeech(Object.Object):
    """mix-in class with prompt to save speech files

    **INSTANCE ATTRIBUTES**

    *BOOL prompt_to_save_speech_files* -- flag indicating whether or not 
    the user should be prompted to save speech files
    """
    def __init__(self, prompt = 1, **args):
        """
        *BOOL prompt* -- true if the user should be prompted, false if
        not
        """
        self.deep_construct(SaveSpeech, 
                 {'prompt_to_save_speech_files': prompt}, args)

    def should_prompt_save_speech_files(self, prompt = 1):
        """sets a flag indicating whether or not the user should be
        prompted to save speech files

        **INPUTS**

        *BOOL prompt* -- true if the user should be prompted, false if
        not

        **OUTPUTS**

        *none*
        """
        self.prompt_to_save_speech_files = prompt
        
    def prompt_save_speech_files(self, frame, allow_cancel = 1):
        """prompts the user to save speech files and other configuration 
        files before exiting, or to cancel.   Note:
        prompt_save_speech_files should save if the user so indicates

        **INPUTS**

        *BOOL allow_cancel* -- true to allow the user to cancel exiting,
        false if the message loop has exited and we must quit

        *wxFrame frame* -- frame above which to display the dialog
        
        **OUTPUTS**

        *BOOL* -- true if the user saved or told the mediator to quit
        without saving, false if the user cancelled.
        """
        if not self.prompt_to_save_speech_files:
            return 1
        flags = wxICON_EXCLAMATION | wxYES_NO | wxNO_DEFAULT
        if allow_cancel:
            flags = flags | wxCANCEL
        answer = wxMessageBox("Save speech files?", "Exiting", flags, frame)
        if answer == wxCANCEL:
            return 0
        if answer == wxYES:
            self.save_speech_files()
        return 1
 
    def save_speech_files(self):
        if sr_interface.sr_user_needs_saving:
            sr_interface.saveUser()


# defaults for vim - otherwise ignore
# vim:sw=4
