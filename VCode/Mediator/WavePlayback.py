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

from Object import Object, OwnerObject

class WavePlayback(OwnerObject):
    """abstract base class for object providing playback of a Wave
    sound-file data

    **INSTANCE ATTRIBUTES**

    *STR data* -- the binary wave data in the form of a Python
    string
    """
    def __init__(self, data, **attrs):
        """
        **INPUTS**

        *STR data* -- the binary wave data in the form of a Python
        string
        """
        self.deep_construct(WavePlayback,
            {'data': data}, attrs)

    def remove_other_references(self):
        self.data = None

    def check(self):
        """check the wave data to see if it is okay

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the wave data appears playable
        """
        debug.virtual('WavePlayback.check')

    def play(self):
        """play the wave data synchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the wave data was played successfully
        """
        debug.virtual('WavePlayback.play')

