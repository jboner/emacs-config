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

#
# Used to generate a list of "public" (i.e. documented) symbols for the
# standard Python libraries.
#
# Feed the .tex documentation file contained in the Python distribution and
# parse all the symbol entries.
#

import glob, re, sys

def re_tex_markup_desc(name):
    """Returns a regexp that matches a particular tex markup."""

    regexp = '\\\\begin\\{%sdesc(ni){0,1}\\}(\\{([^\\}]*)\\})(\\{([^\\}]*)\\})*' % name        
    return regexp

def re_tex_markup_line(name):
    regexp = '\\\\%sline(for_compatibility_with_other_regexp){0,1}(\\{([^\\}]*)\\})(\\{([^\\}]*)\\})*' % name
    return regexp

def re_tex_markup_style3(name):
    regexp = '\\\\%s(for_compatibility_with_other_regexp){0,1}(\\{([^\\}]*)\\})(\\{([^\\}]*)\\})*' % name
    return regexp


def find_symbols(markup, content, symbol_list):
    
    """Finds all symbols mentioned in a particular tex markup, and
    add them to the list"""


    for regexp in (re_tex_markup_desc(markup), re_tex_markup_line(markup), re_tex_markup_style3(markup)):
#        print '-- find_symbols: regexp=\'%s\'' % regexp        
        match_list = re.findall(regexp, content)
#        print '-- find_symbols: match_list=%s' % match_list
        for a_match in match_list:
#            print '-- find_symbols: a_match = %s' % repr(a_match)
            symbol_string = a_match[2] + ', ' + a_match[4]
            symbols = re.split('[^a-zA-Z_]+', symbol_string)
#            print '-- find_symbols: symbol_string=\'%s\', symbols=%s' % (symbol_string, symbols)
            for a_symbol in symbols:
                symbol_list[a_symbol] = 1


def find_module_names(content, symbol_list):
    regexp = '\\\\section\s*\{\s*\\\\module\s*\{([^\}]*)\}'
    match_list = re.findall(regexp, content)
#    print '-- find_module_names: regexp=\'%s\'' % regexp
#    print '-- find_module_names: match_list=%s' % match_list
    for a_match in match_list:
#        print '-- find_module_names: a_match=%s' % a_match        
        sub_modules = re.split('[^a-zA-Z_]+', a_match)
        for a_module in sub_modules:
             symbol_list[a_module] = 1
    

#
# Those symbols are added explicitly because for some reason, they are not in
# any .tex file
#
symbol_list = {'__builtins__': 1, '__coerce__': 1, '__complex__': 1,
               '__debug__': 1, '__delattr__': 1, '__divmod__': 1, '__doc__': 1,
               '__file__': 1, '__float__': 1, '__getinitargs__': 1,
               '__getstate__': 1, '__hex__': 1, '__init__': 1, '__int__': 1,
               '__inv__': 1, '__long__': 1, '__module__': 1, '__name__': 1,
               '__not__': 1, '__oct__': 1, '__pow__': 1, '__radd__': 1, '__rand__': 1,
               '__rdiv__': 1, '__rdivmod__': 1, '_rlshift__': 1,
               '__rmod__': 1, '__rlshift__': 1, '__rmul__': 1, '__ror__': 1, '__rpow__': 1,
               '__rrshift__': 1, '__rsub__': 1,  '__rxor__': 1,
               '__setattr__': 1             
               }


file_list = glob.glob(sys.argv[1])
for a_file_name in file_list:

#    print 'processing file \'%s\'' % a_file_name
    a_file = open(a_file_name, 'r')
    content = a_file.read()
#    print '-- content=%s' % content

    find_symbols('data', content, symbol_list)
    find_symbols('exc', content, symbol_list )   
    find_symbols('func', content, symbol_list)
    find_symbols('class', content, symbol_list)
    find_symbols('method', content, symbol_list)
    find_symbols('member', content, symbol_list)
    
    
    find_module_names(content, symbol_list)


symbols = symbol_list.keys()
symbols.sort()
for a_symbol in symbols:
    print a_symbol
    
    
    
