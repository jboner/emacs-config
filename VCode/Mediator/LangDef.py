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

from Object import Object


class LangDef(Object):
    """Specifications for a given programming language.
    
    **INSTANCE ATTRIBUTES**
    
    *ANY name=None* -- name of the programming language
    
    *ANY regexp_symbol=None* -- a regepx that matches a valid symbol
    
    *ANY regexps_no_symbols=None* -- a regexp that matches portions of
     code that don't contain symbols (e.g. quoted strings, comments)
    

    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, name=None, regexp_symbol=None, regexps_no_symbols=None, **attrs):
        self.deep_construct(LangDef, \
                            {'name': name, \
                             'regexp_symbol': regexp_symbol, \
                             'regexps_no_symbols': regexps_no_symbols}, \
                            attrs, \
                            {})
