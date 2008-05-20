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

"""Interface to a buffer in a fast programming environment (i.e. no
need for caching)"""


import SourceBuff
import sb_mixins 

class SourceBuffNonCached(sb_mixins.WithStateService, 
    SourceBuff.SourceBuffWithServices):
    
    """Interface to a a buffer in a fast programming environment
    (i.e. no need for caching)

    This [SourceBuff] subclass should be used in situations when the
    external editor communicates with VoiceCode through a fast
    link. In such cases, we can afford to always read the state
    directly from the external editor.

    **INSTANCE ATTRIBUTES**
    
    *none*-- 

    CLASS ATTRIBUTES**
    
    *none* -- 

    ..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""
    
    def __init__(self, **args_super):
        self.deep_construct(SourceBuffNonCached, 
                            {}, 
                            args_super, 
                            {})
    def remove_other_references(self):
#        sb_mixins.WithStateService.remove_other_references(self)
        SourceBuff.SourceBuffWithServices.remove_other_references(self)

