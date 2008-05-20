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

"""Basic implementation of a restore-able SourceBuff state"""

import debug
import re, string, sys

from Object import Object
from SourceBuffCookie import SourceBuffCookie

class SourceBuffState(SourceBuffCookie):
    """Basic implementation of a restore-able SourceBuff state

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *STR* buff_name -- name of buffer

    *STR* text -- copy of the buffer contents

    *INT* cursor_at -- indicates whether the cursor is at the start (0)
    or end (1) of the selection_range

    *(INT, INT)* selection_range -- range of the selection
    
    """
    
    def __init__(self, buff_name, contents, selection, cursor_at = 1,
                 last_search = None, **attrs):
        self.deep_construct(SourceBuffState,
                            {'text': contents,
                            'buff_name': buff_name,
                            'selection_range': selection,
                            'cursor_at_end': cursor_at,
                             'logged_search': last_search},
                            attrs
                            )

    def rename_buffer_cbk(self, new_buff_name):
        """callback which notifies us that the application
        has renamed the buffer corresponding to this cookie

        **INPUTS**

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        self.buff_name = new_buff_name

    def name(self):
        """returns buff_name

        **INPUTS**

        *none*
        
        **OUTPUTS**

        *STR* -- file name
        """
        return self.buff_name

    def get_selection(self):
        """retrieves range of stored selection.  

        **INPUTS**

        *none*
        
        **OUTPUTS**

        *(INT, INT)* -- (start, end)

        start is the offset into the buffer of the start of the current
        selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).
        """
        return self.selection_range

    def position(self):
        """returns the stored location of the cursor

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- offset into the buffer of the stored cursor location
        """
        return self.selection_range[self.cursor_at()]

    def cursor_at(self):
        """tells at which end of the selection the cursor was located

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- 1 if cursor is at the end of the selection, 0 if it is
        at the start
        """
        return self.cursor_at_end

    def contents(self):
        """returns stored contents

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- contents of the buffer
        """
        return self.text
    def last_search(self):
        """returns last logged search

        **INPUTS**

        *none*

        **OUTPUTS**

        *(regex, direction, where, match)*

        where

        *STR* regexp -- Regular expreesion used for the search.
        
        *BOOL* direction -- If negative, then we were looking
         backwards. Forward if positive. If *None*, then we were doing
         a *Select Pseudocode* operation and we didn't care about
         direction.
        
        *INT* where -- If positive, then we wanted to put cursor after
         occurence. Before occurence if negative.
        
        *(INT, INT)* match -- Start and end position of the match that was
        used.
        """
        return self.logged_search
        
      
