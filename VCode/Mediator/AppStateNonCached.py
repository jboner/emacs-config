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

"""Interface to a fast programming environment (i.e. no need for caching)"""

import AppState, SourceBuffNonCached

class AppStateNonCached(AppState.AppState):
    """Interface to a fast programming environment (i.e. no need for caching)

    This [AppState] subclass should be used in situations when the
    external editor communicates with VoiceCode through a fast
    link. In such cases, we can afford to always read the state
    directly from the external editor.

    **INSTANCE ATTRIBUTES**
    
    *none*-- 

    CLASS ATTRIBUTES**
    
    *none* -- 

    ..[AppState] file:///./AppState.AppState.html"""
    
    def __init__(self, **args_super):
        self.deep_construct(AppStateNonCached, 
                            {}, 
                            args_super,
                            {})


    #
    # Here will be 'pass' versions of the various editor callback functions
    # used to update VoiceCode's copy of the buffers.
    #
    # Note that we can't just override apply_updates to do nothing because
    # apply_updates changes the V-E map as well as the local copy of the
    # buffer.
    #


