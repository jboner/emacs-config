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

"""abstract interfaces for dictation and selection grammars
"""

from wxPython.wx import *
from WavePlayback import WavePlayback

try:
    wxSound
except NameError:
    wxSound = None
    

class WavePlaybackWX(WavePlayback):
    """implementation of WavePlayback using the wxWave class of wxPython
    """
    def __init__(self, **attrs):
        self.deep_construct(WavePlaybackWX,
            {'wave': None}, attrs)
        if self.data:
# AD: Use one or the other depending on version of wxPython 2.5 used
# trying this for different versions, QH:
            if not wxSound is None:
                wave = wxSound()
                wave.CreateFromData(self.data)
            else:
                wave = wxWaveData(self.data)
            if wave.IsOk():
                self.wave = wave

    def remove_other_references(self):
        self.wave = None
        WavePlayback.remove_other_references(self)

    def check(self):
        """check the wave data to see if it is okay

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the wave data appears playable
        """
        if self.wave and self.wave.IsOk():
            return 1
        return 0

    def play(self):
        """play the wave data synchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the wave data was played successfully
        """
        if self.check():
            self.wave.Play(0) # play synchronously
            return 1
        return 0

