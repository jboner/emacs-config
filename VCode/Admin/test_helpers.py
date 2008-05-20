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

import util

def failed_test_assertion(err):
   print "\n\n>>> Test assertion failed with exception:"
   print err 
   print "\n\n"    

def compilation_test(interp, source):
    
    """Does a compilation test on file *source*        
    """
    print '*** Compiling symbols from file: %s ***' % util.within_VCode(source)
    interp.cleanup_dictionary()
    interp.parse_symbols_from_file(source)
    print '\n\nParsed symbols are: '
    interp.print_symbols()
    print 'Unresolved abbreviations are:'
    unresolved = interp.peek_at_unresolved()
    sorted_unresolved = unresolved.keys()
    sorted_unresolved.sort()
    for an_abbreviation in sorted_unresolved:
        symbol_list = unresolved[an_abbreviation].keys()
        symbol_list.sort()
        print '\'%s\': appears in %s' % (an_abbreviation, str(symbol_list))
        
    print '\n*** End of compilation test ***\n'
    
