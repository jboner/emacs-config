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
import debug
import symbol_formatting
import util

class SymbolResult(Object):
    """
    class representing a portion of an utterance translated as a
    new or existing symbol
    """
    def __init__(self, native_symbol, spoken_phrase, exact_matches,
                 as_inserted, buff_name,
                 builder_preferences, possible_matches = None,
                 forbidden = None,
                 new_symbol = 0, in_utter_interp = None, **args):
        """
        ** INPUTS **

        *STR native_symbol* -- the written form of the symbol

        *[STR] spoken_phrase* -- the list of spoken words which were
        translated into this symbol

        *STR buff_name* -- the name of the buffer in which the symbol
        was dictated

        *TextBlock as_inserted* -- the text as inserted  (possibly
        including leading or trailing spaces) and the range in the
        buffer this text occupied just after insertion
        
        *[STR] builder_preferences* -- list of names of
        registered SymBuilder objects, prioritized according to the
        state of the interpreter at the time the symbol was
        interpreted.

        *[STR] exact_matches* -- a prioritized list of exact matches
        to known symbols

        *BOOL new_symbol* -- true if the symbol was a new symbol,
        false if it matched an existing symbol
        
        *[(INT, STR)] possible_matches* -- list of (confidence score,
        written_form) tuples for possible (but not exact) matches to 
        the spoken form of this symbol.
        
        *[(INT, STR)] forbidden* -- list of (confidence score,
        written_form) tuples for forbidden inexact matches (but
        which may be displayed as alternatives in the exact symbols
        tab of the re-formatting dialog)
        
        *UtteranceInterpretation in_utter_interp = None* -- Utterance 
        interpretation in which that symbol was heard.
        
        *STR reformatted_to=None* -- alternate form that this symbol was reformatted
        to.
        """
        self.deep_construct(SymbolResult,
                            {
                             'symbol': native_symbol,
                             'phrase': spoken_phrase,
                             'buff_name': buff_name, 
                             'text': as_inserted,
                             'builders': builder_preferences,
                             'exact': exact_matches,
                             'possible': possible_matches,
                             'forbidden': forbidden,
                             'was_new': new_symbol,
                             'in_utter_interp': in_utter_interp,
                             'reformatted_to': None,
                            }, args)
                            
    def native_symbol(self):
        return self.symbol

    def buffer(self):
        return self.buff_name

    def final_range(self):
        return self.location

    def spoken_phrase(self):
        return self.phrase

    def builder_preferences(self):
        return self.builders

    def new_symbol(self):
        """
        Indicates whether the symbol was a newly generated symbol, or
        was a match to a previously known symbol
        """
        return self.was_new
        
    def exact_matches(self):
        """
        Returns a prioritized list of exact matches to known
        symbols

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- written forms of known symbols which are an exact
        match to the spoken form of this symbol
        """
        return self.exact
        
    def possible_matches(self):
        """
        Returns a prioritized list of possible (but not exact) matches
        to known symbols

        **INPUTS**

        *none*

        **OUTPUTS**

        *[(INT, STR)]* -- the confidence score and written forms of 
        possible matches, or None if none have been generated yet
        """
        return self.possible
    
    def forbidden_matches(self):
        """
        Returns a prioritized list of possible (but not exact) matches
        to known symbols

        **INPUTS**

        *none*

        **OUTPUTS**

        *[(INT, STR)]* -- the confidence score and written forms of 
        forbidden matches, or None if none have been generated yet
        """
        return self.forbidden
        
    def possible_new_symbol_formats(self):
       """returns a list of possible formats for the symbol if it was
       to be created as a new symbol"""
       formats = []
       for a_style in self.builder_preferences():
          sym_builder = symbol_formatting.registry.make_builder(a_style)
          debug.trace('SymbolResult.possible_new_symbol_formats', '** sym_builder=%s' % sym_builder)
          sym_in_this_format = sym_builder.build_from_words(self.spoken_phrase())
          debug.trace('SymbolResult.possible_new_symbol_formats', '** sym_in_this_format=%s' % sym_in_this_format)
          formats.append(sym_in_this_format)
       return formats    
        
    def suggestions_list(self):
        """returns a prioritized list of written forms that COULD 
        be used for the symbol. This list is displayed to the user
        in the symbol reformatting dialog.
        
        We first list exact matches, then possible matches (sorted 
        in decreasing order of likelihood), then formats for 
        the symbol if it is considered as a new one.
        
        **OUTPUTS**
        
        *[STR]* -- The list from most likely to least likely.
        """
        debug.trace('SymbolResult.suggestions_list', '** self.native_symbol()=%s, self.exact_matches()=%s' %(self.native_symbol(), self.exact_matches()))
        list = [self.native_symbol()] + \
               util.remove_occurences_from_list(self.native_symbol(), self.exact_matches())
       
        sorted_possible = self.possible_matches()
        def cmp(a, b):
           if a[0] > b[0]: return 1
           if a[0] < b[0]: return -1
           return 0

        # the case sorted_possible is None (QH):
        if sorted_possible != None:
            sorted_possible.sort(cmp)        
            sorted_possible.reverse()
            for a_possible in sorted_possible:
                if not a_possible[1] in list:
                    list.append(a_possible[1])
   
        for a_new_format in self.possible_new_symbol_formats():
           if not a_new_format in list:
              list.append(a_new_format)
        
        return list
        
        
    def reformat_to(self, alt_form):
       """Changes the written form of the symbol to an alternate form.
       
       Note: This does not reinterpret the symbol.
       
       **INPUTS**
       
       *STR alt_form* -- the alternate form.
       """
       self.reformatted_to = alt_form
        
    def cleanup(self):
        self.in_utter_interp = None
