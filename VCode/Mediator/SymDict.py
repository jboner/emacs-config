#############################################################################
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

"""Classes for dealing with programming symbols and their spoken forms.
"""


from Object import Object, OwnerObject
import sb_services, SourceBuff
from LangDef import LangDef
import auto_test, PickledObject, sr_interface, vc_globals
import WordTrie
from SymbolResult import SymbolResult
import DictConverter
import util
from debug import trace, tracing
import debug
import StringIO
import math
import copy

import copy, cPickle, exceptions, os, re, string, sys
import shutil
import stat
import time
import os.path
import shelve

import traceback

language_definitions={}

current_version = 5

#
# Minimum length for an abbreviation (short abbreviations tend to introduce
# too many false spoken forms, which bloats the SR's vocabulary)
#
min_abbreviation_len = 2

#
# Set *vocabulary_symbols_written_form* to 1 if you want VoiceCode to create a
# written form/spoken form entry in the SR vocabulary for every symbol it
# compiles.
# If set to 0, SR will create a spoken form entry only and the VoiceCode
# command interpreter will translate that spoken form to a written form
#

vocabulary_symbols_with_written_form = 1


def pluralize(word):
    """Finds the plural form of a word
        
    **INPUTS**
        
    *STR* word -- the word to be pluralized 
        

    **OUTPUTS**
        
    *STR plural* -- plural of *word*
    """        
    #
    # For now, just append an 's'
    #
    return word + 's'

class SymbolInfo(Object):
    """Stores information about a parsed symbol.
    
    **INSTANCE ATTRIBUTES**
    
    *[STR] spoken_forms=[]* -- list of spoken forms for that symbol.

    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, spoken_forms = None, **attrs):
        self.deep_construct(SymbolInfo, \
                            {'spoken_forms': []}, \
                            attrs, \
                            {})
        if spoken_forms:
            self.spoken_forms.extend(spoken_forms)
            
    def add_spoken_forms(self, spoken_forms):
        if tracing('SymbolInfo.add_spoken_forms'):
            trace('SymbolInfo.add_spoken_forms', 'spoken_forms=%s' % repr(spoken_forms))
        for form in spoken_forms:
            if not (form in self.spoken_forms):
                self.spoken_forms.append(form)
            
    def remove_spoken_form(self, spoken_form):
        self.spoken_forms.remove(spoken_form)




# NOTE: This class is no longer used by SymDict, but must be kept
# because it is used by the persistent storage of SymDict before 
# version 4.  
class SpokenFormInfo(Object):
    """Stores information about a spoken form for a parsed symbol.
    
    **INSTANCE ATTRIBUTES**
                        
    *[STR] symbols=[]* -- list of symbols (written forms) that have
    this spoken form.
    
    CLASS ATTRIBUTES**
            
    *none* -- 
    """
            
    def __init__(self, symbols=[], **attrs):
        self.deep_construct(SpokenFormInfo, \
                            {'symbols': symbols}, \
                            attrs, \
                            {})

class SymbolMatch(Object):
    
    """Encapsulates information about a match between a pseudo-symbol
    and a native symbol.
            
    **INSTANCE ATTRIBUTES**
            
    *STR pseudo_symbol=None* -- The pseudo symbol (e.g. "a new symbol")
    
    *STR native_symbol=None* -- The matched native symbol (e.g. aNewSym)

    *STR words=None* --  The words in *pseudo_symbol*
    
    *BOOL is_new=0* -- If true, then the symbol is a new one

    *NUM fmt_rank=10* -- For new symbol, this gives the match's rank in
     the list of possible forms for the new symbol
    
    *[STR] word_matches=None* -- List of words matching each segment of
    native_symbol.

    CLASS ATTRIBUTES**
            
    *none* -- 
    """
    
    def __init__(self, pseudo_symbol=None, native_symbol=None, words=None, word_matches=None, is_new=0, fmt_rank=10, **args_super):
        if tracing('SymbolMatch.__init__'):
            trace('SymbolMatch.__init__', 'word_matches=%s' % repr(word_matches))
        self.deep_construct(SymbolMatch, \
                            {'pseudo_symbol': pseudo_symbol, \
                             'native_symbol': native_symbol, \
                             'words': words, \
                             'word_matches': word_matches, \
                             'is_new': is_new, \
                             'fmt_rank': fmt_rank}, \
                            args_super, \
                            {})



    def score(self):
        """Returns a score for the match.
        
        **INPUTS**
        
        *none* -- 
        
        **OUTPUTS**
        
        *INT score* -- The score takes into account things like:
        - the likelyhood of the abbreviations (presumably) used in *native_symbol* (not implemented yet)
        - the length of *native_symbol*
        """
                         
        if self.is_new:
            #
            # For matches to new symbols, score depends on the rank of the
            # format used to create the symbol
            #
            # Note use of float to force float division (as opposed to integer
            # div)
            #
            score = float(1)/self.fmt_rank
        else:
            #
            # For matches to existing symbols, just base score on length of
            # the native symbol for now
            #
            score = len(self.native_symbol)

        return score

    

    def compare_scores(self, other_match):
        """Compares the score of a match to that of an other match.

        Note that since the first argument is *self*, this method can
        be passedas a comparison method to *LIST.sort* method.
        
        **INPUTS**
        
        *SymbolMatch* other_match -- The other match to compare *self* to.
        

        **OUTPUTS**
        
        *INT* compare_flag -- -1 -> *self* has lower score
                               0 -> *self* has same score
                               1 -> *self* has higher score
        """

        self_score = self.score()
        other_score = other_match.score()
        if self_score < other_score:
            compare_flag = -1
        elif self_score > other_score:
            compare_flag = 1
        else:
            compare_flag = 0

        return compare_flag


class ErrorReadingPersistDict(RuntimeError):
    def __init__(self, message):
        RuntimeError.__init__(self, message)
        self.message = message


class SymDict(OwnerObject):
    """Known symbols dictionary.

    This class stores information about symbols defined in source files
    that the user is working on.

    It has methods for parsing symbols and adding pronounceable
    phrases for those symbols to the Speech Recognition system's vocabulary.

    Also has methods for matching a pseudo symbol to a native symbol
    (e.g. "a new symbol" -> aNewSym)

    **INSTANCE ATTRIBUTES**

    *CmdInterp interp* -- reference to the parent CmdInterp

    *{STR:* [SymbolInfo] *)}* symbol_info={} -- Dictionary of known
     symbols. Key is the native written form of the symbol and the
     value is the information about that symbol.

    *WordTrie* spoken_form_info -- WordTrie data structure which maps
     spoken forms to written forms of known symbols.

    *{STR: 1}* tentative_symbols -- set of symbols which have been added
    tentatively, but will be removed if the user undoes or corrects the 
    utterance containing them, or re-formats them.  The keys are the
    native written forms of the tentative symbols.  Tentative symbols
    become permanent when they are inserted by re-formatting, or when
    the user exists the mediator.

    *{STR: [STR]}* abbreviations -- Dictionary of preferred
     abbreviations for words.  The key is the word, and the value is a
     prioritized list of abbreviations

    *{STR: [STR]}* alt_abbreviations -- Dictionary of alternative
     abbreviations to be used as additional suggestions in the sing
     symbol reformatting dialog, but not as the first choice in initial
     dictation of new symbols.  The key is the word, and the value is a
     prioritized list of abbreviations.  The inverse of this mapping is 
     also used to generate expansions for abbreviations in symbols 
     scanned from existing code.  In fact, that is the primary purpose
     of the alternative abbreviations, but they are stored in the
     inverse form because the order of abbreviations for a given word is
     significant in the reformatting dialog, whereas the order of
     expansions for a given abbreviations is not.

    *{STR: [STR]}* pronunciations -- dictionary of pronunciations for
    abbreviated words.  These pronunciations are used to generate
    expansions for components of known symbols, but not
    abbreviations.  For example, we might want to tell the mediator to
    use 'deaf' in the spoken form of a symbol containing the word 'def',
    but wouldn't want 'deaf' to be abbreviated as 'def' in new symbols.

    *{STR: INT}* acronyms -- set of acronyms which should be expanded
    when seen as components of known symbols

    *{STR: [STR]}* extra_expansions={} -- Dictionary of
     extra expansions added manually.

    *{STR: [STR]}* expansions={} -- Dictionary of
     expansions of abbreviations. The key is the abbreviation and 
     the value is a list of possible expansions.  This dictionary is
     generated by combining the inverses of abbreviations and
     alt_abbreviations with the expansions generated from
     pronunciations, acronyms, and extra expansions.

    *{STR: {STR: 1}}* unresolved_abbreviations={} -- Dictionary of
     unresolved abbreviations. These are abbreviations that have
     appeared in at least one compiled symbol, yet are neither a word
     in the speech vocabulary or a known abbreviation. Values are
     dictionnaries that list the symbols containing the unresolved
     abbreviation.

    *{STR: STR}* _cached_symbols_as_one_string -- Caches the last value
     returned by method [symbols_as_one_string] for each initial letter.
     A value of *None* indicates that the string needs to be regenerated by
     [symbols_as_one_string].

    *{STR: {STR: 1}* _symbols_starting_with -- Caches the written forms of
    all symbols starting with a given letter as a set

    *[STR] standard_symbol_sources* -- List of files in which standard
    symbols for different languages are defined.

    *[STR] symbol_sources_read* -- List of files from which standard
    symbols have already been scanned.

    *[STR] abbrev_sources* -- List of files in which
    abbreviations and expansions for symbol terms are defined.

    *STR* sym_file = None -- Name of the file used to store those SymDict 
    attributes which should be persistent.  If *None*
    it means the object should never be saved to/read from file.

    *STR* export_file -- name of the default file to which
    abbreviations are exported (without the trailing .py, so that we can
    append .on_init)

    *BOOL* from_file -- true if the symbol dictionary was successfully
    loaded from a file

    *INT* file_time -- if from_file is true, contains the time of last 
    modification of the symbol dictionary file (in seconds since the
    epoch, as returned by os.stat).  Otherwise, file_type is None.

    [SB_ServiceLang] *lang_name_srv* -- Service used by SymDict to
    determine a buffer's programming language.
    
    [INT] *min_chars_for_approx_match=4* -- Minimum number of characters that a
    native symbol must have in order to be matched approximately to a
    pseudo symbol. Very short symbols tend to cause too many false positive
    matches, and in any case, they are easy to dictate by spelling (in fact
    the easiest way to dictate a short symbol is to spell it).
    
    [INT] *min_non_consec_chars_for_approx_match=3* -- minimum number of
    characters from each word which must appear in a word of a native
    symbol, unless the word of the native symbol is a prefix of the
    spoken word

    [INT] *min_chars_run_together=3* -- Minimum number of characters
    for each run-together word that an abbreviation found by match_pseudo_symbol
    must have in order to be accepted as an approximate match to a
    pseudo symbol. 
   
    [INT] *max_auto_acronym=3* -- Maximum number of characters
    in one word of a symbol for us to automatically generate an acronym
    as part of the expanded forms

    *{STR: [STR]} weak_pairs* -- pairs of weak and strong consonants
     such that the first member of the pair is often dropped from a
     symbol while the second is retained.  The keys are the weak
     consonants, and the value is the list of strong consonants
     associated with each weak consonant.

    *FLOAT match_threshold* -- Default minimum confidence level required of an
    approximate symbol match

    *FCT BOOL(STR, STR) word_exists* -- alternative function to use to
    check whether a word exists, or None to use self.std_word_exists.
    Used only in regression testing, to standardize behavior across
    different versions of the speech engine vocabulary
   
    CLASS ATTRIBUTES**

    *{STR: * [LangDef] *}* language_definitions={} -- Key is the name
     of a language and the value is a language definition object which
     defines rules for parsing symbols in that language.
        
    .. [LangDef] file:///./LangDef.LangDef.html
    .. [SymbolInfo] file:///./SymDict.SymbolInfo.html
    .. [symbols_as_one_string] file:///./SymDict.SymDict.html#symbols_as_one_string"""

    def __init__(self, sym_file = None, interp = None, 
        export_file = None, match_threshold = 0.4, **attrs):

        # These attributes can't be set with constructor arguments
        self.decl_attrs({'_cached_symbols_as_one_string': {}, 
                         '_symbols_starting_with': {}, 
                         'spoken_form_info': WordTrie.WordTrie(), 
                         'symbol_info': {}, 
                         'tentative_symbols': {}, 
                         'abbreviations': {}, 
                         'alt_abbreviations': {}, 
                         'expansions': {}, 
                         'pronunciations': {}, 
                         'acronyms': {}, 
                         'extra_expansions': {}, 
                         'standard_symbol_sources': [], 
                         'symbol_sources_read': [], 
                         'abbrev_sources': [], 
                         'unresolved_abbreviations': {}, 
                         # AD: This is ugly... should use the AppState's SB_SereviceLang
                         #     instance instead.
                         'lang_name_srv': sb_services.SB_ServiceLangServerSide(buff=None), 
                         'min_chars_for_approx_match': 4, 
                         'min_chars_run_together': 4, 
                         'min_non_consec_chars_for_approx_match': 3, 
                         'max_auto_acronym': 3, 
                         'word_exists': None, 
                         'common_hyphenated': {'un': 0 , 'co': 0, 'non': 0, 
                             're': 0}, 
                         'weak_pairs': {'n': ['t']}
                        })
        
        self.deep_construct(SymDict, 
                            {'sym_file': sym_file, 
                             'export_file': export_file, 
                             'interp': interp, 
                             'from_file': 0, 
                             'file_time': None, 
                             'match_threshold': match_threshold}, 
                            attrs)
        self.name_parent('interp')
        if not export_file:
            self.export_file = os.path.join(vc_globals.state, 'abbrevs')
        self.from_file = self.init_from_file()

#        print '-- SymDict.__init__: returning self.__dict__=%s' % self.__dict__

    def export_abbreviations(self, file):
        """export abbreviation preferences to a text file

        **INPUTS**

        *STR file* -- the name of the output file

        **OUTPUTS**

        *BOOL* -- true if the abbreviations were successfully written to
        the file
        """
        try:
            backup = None
            if os.path.exists(file):
                backup, ext = os.path.splitext(file)
                if ext == '.bak':
                    backup = file + '.bak'
                else:
                    backup = backup + '.bak'
                shutil.copyfile(file, backup)
            f = open(file, "w")
        except:
            return 0
        return self.export_to_file(f)

    def export_to_file(self, f):
        """export abbreviation preferences to an open text file

        **INPUTS**

        *file f* -- an empty file open for writing

        **OUTPUTS**

        *BOOL* -- true if the abbreviations were successfully written to
        the file
        """
        try:
            lines = {}
            words = self.abbreviations.keys()
            for word in words:
                cmd = 'add_abbreviations(%s, %s)\n' \
                    % (repr(word), repr(self.abbreviations[word]))
                lines[word] = [cmd]
            words = self.alt_abbreviations.keys()
            for word in words:
                cmd = 'add_alt_abbreviations(%s, %s)\n' \
                    % (repr(word), repr(self.alt_abbreviations[word]))
                try:
                    lines[word].append(cmd)
                except KeyError:
                    lines[word] = [cmd]
            words = lines.keys()
            words.sort(lambda a, b: cmp(string.lower(a), string.lower(b)))
            for word in words:
                f.writelines(lines[word])
            f.write('\n')
                
            lines = {}
            abbreviations = self.pronunciations.keys()
            for abbreviation in abbreviations:
                pronunciations = self.pronunciations[abbreviation]
                pronunciations.sort()
                lines[abbreviation] = []
                for pronunciation in pronunciations:
                    cmd = 'add_pronunciation(%s, %s)\n' \
                        % (repr(abbreviation), repr(pronunciation))
                    lines[abbreviation].append(cmd)
            abbreviations = self.extra_expansions.keys()
            for abbreviation in abbreviations:
                expansions = self.extra_expansions[abbreviation]
                expansions.sort()
                for expansion in expansions:
                    cmd = 'add_extra_expansion(%s, %s)\n' \
                        % (repr(abbreviation), repr(expansion))
                    try:
                        lines[abbreviation].append(cmd)
                    except KeyError:
                        lines[abbreviation] = [cmd]
            abbreviations = self.acronyms.keys()
            for abbreviation in abbreviations:
                cmd = 'add_acronym(%s)\n' % repr(abbreviation)
                try:
                    lines[abbreviation].append(cmd)
                except KeyError:
                    lines[abbreviation] = [cmd]
            abbreviations = lines.keys()
            abbreviations.sort()
            abbreviations.sort(lambda a, b: cmp(string.lower(a), 
                string.lower(b)))
            for abbreviation in abbreviations:
                f.writelines(lines[abbreviation])
            f.close()
            return 1
        except:
            return 0

    def persistent_dict(self):
        """creates the Python dictionary which will be stored as a
        persistent version of the symbol dictionary.

        **INPUTS**

        *none*
        
        **OUTPUTS**
        
        *{STR: ANY}* -- the dictionary.  Note that this dictionary
        contains references to attributes of self, so it should not be
        modified.
        """
        d = {}
# any objects stored must be correctly unpickled even if their classes
# are modified.
        d['version'] = current_version
        d['symbol_info'] = self.symbol_info
# at the moment, spoken_form_info is redundant with symbol_info, except for 
# ordering, but I'm not sure that will always be true, or what to do 
# about ordering, so for now let's just store both
        d['spoken_form_info'] = self.spoken_form_info
        d['abbreviations'] = self.abbreviations
        d['alt_abbreviations'] = self.alt_abbreviations
        d['acronyms'] = self.acronyms
        d['pronunciations'] = self.pronunciations
        d['extra_expansions'] = self.extra_expansions
        d['abbreviations'] = self.abbreviations
        d['unresolved_abbreviations'] = self.unresolved_abbreviations
        d['symbol_sources_read'] = self.symbol_sources_read
        return d

    def save(self, file = None, export_abbreviations = 1):
        """save persistent attributes to a file

        **INPUTS**

        *STR file* -- name of the file, or None to use the sym_file name
        given at construction.  Normally, this argument should be
        omitted.  It is included only for regression testing of the
        persistence features of SymDict

        *BOOL export_abbreviations* -- if true, export abbreviations to
        text file abbrevs.py.  Ignored if we are running regression
        tests

        **OUTPUTS**

        *none*
        """
        if file is None:
            file = self.sym_file
        if file is None:
            return
        try:
            backup = None
            if os.path.exists(file):
                backup, ext = os.path.splitext(file)
                if ext == '.bak':
                    backup = file + '.bak'
                else:
                    backup = backup + '.bak'
                shutil.copyfile(file, backup)
            f = open(file, "w")
        except:
            msg = 'WARNING: error creating SymDict state file %s\n' % file
            sys.stderr.write(msg)
            return
            
        d = self.persistent_dict()

        try:
            cPickle.dump(d, f)
        except:
            msg = 'WARNING: error writing to SymDict state file %s\n' % file
            sys.stderr.write(msg)
            traceback.print_exc()
            f.close()
            os.remove(file)
            if backup:
                shutil.copyfile(backup, file)
            return

        f.close()
        if self.sym_file and export_abbreviations:
# unless we are running regression tests, export abbreviation
# preferences to a file as well
            export_file = self.export_file + '.py'
            exported = self.export_abbreviations(export_file)
            if not exported:
                msg = 'Warning: failure to export current abbreviation\n'
                msg = msg + 'preferences to abbrevs.py\n'
                sys.stderr.write(msg)

    def known_symbol(self, symbol):
        """indicates whether a given symbol is known

        **INPUTS**

        *STR symbol* -- the written form of the symbol

        **OUTPUTS**

        *BOOL* -- true if the symbol is known
        """
        return self.symbol_info.has_key(symbol)

    def spoken_forms(self, symbol):
        """returns the list of spoken forms of the given symbol 

        **INPUTS**

        *STR symbol* -- the written form of the symbol

        **OUTPUTS**

        *[STR]* -- a copy of the list of spoken forms for the symbol, as
        used in the vocabulary entries, or None if the symbol is unknown
        """
        if self.known_symbol(symbol):
            return copy.copy(self.symbol_info[symbol].spoken_forms)
        return None

    def match_head(self, phrase, use_match_threshold=None):
        """looks for a complete or partial (prefix) EXACT match of the
        phrase to the spoken forms of known symbols.
        returns the value corresponding to the longest prefix of the
        given phrase which appears in the WordTrie

        **INPUTS**

        *[STR] phrase* -- list of words to match
        
         *FLOAT use_match_threshold* -- minimum confidence level required of an
         approximate symbol match. If None, use the default setting of the
         SymDict.


        **OUTPUTS**

        *([STR], [STR], BOOL)* -- First element is the list of written 
        forms of known symbols corresponding to the partial phrase. Second
        element is the list of remaining unmatched words from the phrase.
        Third element specifies whether the match was exact. 
        """
        exact_matches = self.spoken_form_info.match_head(phrase)
        fuzzy_matches = self.fuzzy_match_head(phrase, use_match_threshold)
        debug.trace('SymDict.match_head(', 
                    "** phrase=%s, exact_matches=%s, fuzzy_matches=%s" % 
                    (phrase, exact_matches, fuzzy_matches))
        
        exact_matches_unmatched_words = exact_matches[1]
        fuzzy_matches_unmatched_words = fuzzy_matches[1]
        # Take match which consumes the most words from the phrase
        # In case of tie, exact matches take precedence.
        if (len(exact_matches_unmatched_words) <= len(fuzzy_matches_unmatched_words)):
           match = (exact_matches[0], exact_matches[1], True)
        else:
           match = ([], fuzzy_matches_unmatched_words, False)
           for a_match in fuzzy_matches[0]:
              match[0].append(a_match[1])

        debug.trace('SymDict.match_head(', 
                    "** returning match=%s" % repr(match))
            
        return match
    
    def fuzzy_match_head(self, phrase, use_match_threshold=None):
        """Looks for a complete or partial (prefix) FUZZY match of 
        phrase to the spoken forms of known symbols.
        returns the value corresponding to the longest prefix of the
        given phrase which appears in the WordTrie

        **INPUTS**

        *[STR] phrase* -- list of words to match
        
         *FLOAT use_match_threshold* -- minimum confidence level required of an
         approximate symbol match. If None, use the default setting of the
         SymDict.

        **OUTPUTS**

        *([STR], [STR])* -- the list of written forms of known symbols
        corresponding to the partial phrase, together with any 
        remaining unmatched words from the phrase
        
        See SymDictTest unit test for examples of how to use"""
        
        trace('SymDict.fuzzy_match_phrase', "** phrase=%s, use_match_threshold=%s" % (phrase, use_match_threshold))
        
        longest_matchings_found = (None, copy.copy(phrase))
        if phrase:
           rest_words = copy.copy(phrase)
           match_text = ''
           while len(rest_words) > 0: 
               word = rest_words.pop(0)
               if match_text != '':
                   match_text = match_text + " "
               match_text = match_text + word
               trace('SymDict.fuzzy_match_phrase', 
                     "** match_text='%s', rest_words=%s" %
                     (match_text, rest_words))
            
               symbol_matches, weak_matches, forbidden = \
                   self.match_pseudo_symbol(match_text, use_match_threshold)
            
               trace('SymDict.fuzzy_match_phrase', 
                     "** matches for '%s' are (%s, %s, %s)" %
                     (match_text, symbol_matches, weak_matches, forbidden))
            
               if symbol_matches:
                   longest_matchings_found = \
                      (symbol_matches, copy.copy(rest_words))
                   trace('SymDict.fuzzy_match_phrase', 
                         "** this matches, so now longest_matchings_found =%s" % repr(longest_matchings_found))
            
        return longest_matchings_found

    def complete_match(self, phrase):
        """returns the list of symbols with spoken forms exactly
        matching a given phrase

        **INPUTS**

        *[STR] phrase* -- list of words to match

        **OUTPUTS**

        *[STR]* -- list of written forms of symbols whose spoken 
        forms match the phrase, or None if no match was found
        """
        phrases_list = self.spoken_form_info.complete_match(phrase)
        if not phrases_list:
           phrases_list = []
        return phrases_list
  
    def _add_corresponding_expansion(self, abbreviation, expansion):
        """private method to add expansions for an abbreviation corresponding 
        to the mapping a word to its abbreviation.  

        *NOTE:* This method should only be called by add_abbreviation,
        prepend_abbreviations, etc., because otherwise the expansion
        will be lost when the user exits VoiceCode

        **INPUTS**
        
        *STR* abbreviation -- the new abbreviations

        *STR* expansion -- the word which is an expansion of the
        abbreviation
        
        **OUTPUTS**

        *none*
        """
        if self.expansions.has_key(abbreviation):
            if expansion not in self.expansions[abbreviation]:
                self.expansions[abbreviation].append(expansion)
        else:
            self.expansions[abbreviation] = [expansion]
        if self.unresolved_abbreviations.has_key(abbreviation):
            if tracing('SymDict._add_corresponding_expansion'):
                trace('SymDict._add_corresponding_expansion', 
                        '%s was previously unresolved' % abbreviation)
            if abbreviation[ - 1] == 's':
                single = abbreviation[:  - 1]
            else:
                single = abbreviation
            symbols = self.unresolved_abbreviations[abbreviation].keys()
            if not expansion.startswith(single):
                # if the abbreviation was a prefix of the expansion,
                # then it was likely to be pronouncable, so we leave
                # the spoken forms containing that abbreviation
                self._remove_spoken_forms(symbols, abbreviation)
                
            for a_symbol in symbols:
                if tracing('SymDict._add_corresponding_expansion'):
                    trace('SymDict._add_corresponding_expansion', 
                        'updating forms for %s' % a_symbol)
                spoken_forms = self.get_spoken_forms(a_symbol)
                self.add_symbol(a_symbol, spoken_forms)

            del self.unresolved_abbreviations[abbreviation]
                
    def _remove_spoken_forms(self, symbols, abbreviation):
        """
        Remove the spoken forms containing a given abbreviation from a
        list of symbols

        ** INPUTS **

        *[STR] symbols* -- the written forms of the symbols

        *STR abbreviation* -- the abbreviation indicating which spoken
        forms should be removed
        """
        for a_symbol in symbols:
            symbol_info = self.symbol_info[a_symbol]
            spoken_forms = symbol_info.spoken_forms
            for spoken_form in spoken_forms:
                phrase = self.spoken_to_phrase(spoken_form)
                if abbreviation in phrase:
                    # if this spoken form is affected, then
                    # find all symbols associated with this phrase
                    # and remove the pertinant one
                    phrase_symbols = \
                        self.spoken_form_info.complete_match(phrase)
                    phrase_symbols.remove(a_symbol)
                    # if that leaves an empty phrase match, remove it
                    if not phrase_symbols:
                        self.spoken_form_info.remove_phrase(phrase)
                    # remove the spoken form from the symbol_info
                    # entry as well
                    symbol_info.remove_spoken_form(spoken_form)
                    # and the vocabulary entry
                    self.remove_vocabulary_entry(a_symbol, spoken_form)


    def prepend_abbreviations(self, word, abbreviations):
        """prepends one or more abbreviations to the list of abbreviations 
        for a word, or moves them to the front of the list, if they is 
        already in the list.  Also creates corresponding entries
        in the list of expansions for each abbreviation.

        *NOTE:* this method should only be called if the user has
        explicitly requested import of a user abbreviation file with
        instructions that it should override existing preferences, or by
        the reformatting dialog in accordance with the user's choice

        **INPUTS**

        *STR* word -- the word to be abbreviated
        
        *[STR]* abbreviations -- list of new abbreviations
        
        **OUTPUTS**

        *none*
        """
        #
        # Make sure the word is the SR vocabulary
        #
#        clean_word = sr_interface.clean_spoken_form(word)
#        sr_interface.addWord(clean_word)
        if not self.abbreviations.has_key(word):
            self.abbreviations[word] = abbreviations
        else:
            for abbreviation in abbreviations:
                try:
                    self.abbreviations[word].remove(abbreviation)
                except ValueError:
                    pass
            self.abbreviations[word] = abbreviations + self.abbreviations[word]
        for abbreviation in abbreviations:
            self._add_corresponding_expansion(abbreviation, word)
       
    def add_abbreviation(self, word, abbreviation):
        """Appends an abbreviation to the list of abbreviations for a word, 
        unless it is already present, and creates a corresponding entry
        in the list of expansions for the abbreviation
        
        **INPUTS**

        *STR* word -- the word to be abbreviated
        
        *STR* abbreviation -- the abbreviation 
        
        **OUTPUTS**
        
        *none* -- 
        """
#        clean_word = sr_interface.clean_spoken_form(word)
#        sr_interface.addWord(clean_word)
        if not self.abbreviations.has_key(word):
            self.abbreviations[word] = [abbreviation]
        else:
            try:
                self.abbreviations[word].index(abbreviation)
            except ValueError:
                self.abbreviations[word].append(abbreviation)
        self._add_corresponding_expansion(abbreviation, word)

    def add_abbreviations(self, word, abbreviations):
        """Appends one or more abbreviations to the list of abbreviations 
        for a word, unless they are already present, and creates 
        corresponding entries in the list of expansions for the abbreviation
        
        **INPUTS**

        *STR* word -- the word to be abbreviated
        
        *[STR]* abbreviations -- list of abbreviations to add
        
        **OUTPUTS**
        
        *none* -- 
        """
#        clean_word = sr_interface.clean_spoken_form(word)
#        sr_interface.addWord(clean_word)
        if not self.abbreviations.has_key(word):
            self.abbreviations[word] = abbreviations
        else:
            for abbreviation in abbreviations:
                try:
                    self.abbreviations[word].index(abbreviation)
                except ValueError:
                    self.abbreviations[word].append(abbreviation)
        for abbreviation in abbreviations:
            self._add_corresponding_expansion(abbreviation, word)

    def add_alt_abbreviations(self, word, abbreviations):
        """Appends one or more abbreviations to the list of 
        alternative abbreviations for a word, unless they are already 
        present, and creates corresponding entries in the list of 
        expansions for the abbreviation
        
        **INPUTS**

        *STR* word -- the word to be abbreviated
        
        *[STR]* abbreviations -- list of abbreviations to add
        
        **OUTPUTS**
        
        *none* -- 
        """
#        clean_word = sr_interface.clean_spoken_form(word)
#        sr_interface.addWord(clean_word)
        if not self.alt_abbreviations.has_key(word):
            self.alt_abbreviations[word] = abbreviations
        else:
            for abbreviation in abbreviations:
                try:
                    self.alt_abbreviations[word].index(abbreviation)
                except ValueError:
                    self.alt_abbreviations[word].append(abbreviation)
        for abbreviation in abbreviations:
            self._add_corresponding_expansion(abbreviation, word)

    def add_pronunciation(self, abbreviation, pronunciation):
        """Appends a pronunciation expansion for an abbreviation
        
        **INPUTS**

        *STR* abbreviation -- the abbreviation 
        
        *STR* pronunciation -- the new pronunciation for the
        abbreviation
        
        **OUTPUTS**
        
        *none* -- 
        """
        if not self.pronunciations.has_key(abbreviation):
            self.pronunciations[abbreviation] = [pronunciation]
        else:
            try:
                self.pronunciations[abbreviation].index(pronunciation)
            except ValueError:
                self.pronunciations[abbreviation].append(pronunciation)
        self._add_corresponding_expansion(abbreviation, pronunciation)

    def add_extra_expansion(self, abbreviation, word):
        """adds an extra expansion for an abbreviation
        
        **INPUTS**

        *STR* abbreviation -- the abbreviation 
        
        *STR* word -- the pronouncable expansion
        
        **OUTPUTS**
        
        *none* -- 
        """
        if not self.extra_expansions.has_key(abbreviation):
            self.extra_expansions[abbreviation] = [word]
        else:
            try:
                self.extra_expansions[abbreviation].index(word)
            except ValueError:
                self.extra_expansions[abbreviation].append(word)
        self._add_corresponding_expansion(abbreviation, word)

    def add_acronym(self, acronym):
        """shorthand method for adding an expansion constructed from 
        an acronym (e.g. if acronym is 'csc', then the expansion will 
        be 'C. S. C.')
        
        **INPUTS**

        *STR* acronym -- the acronym to expand.  The original case of
        the acronym is ignored
        
        **OUTPUTS**
        
        *STR* -- the spoken form of the expanded acronym
        """
        acronym = string.lower(acronym)
        self.acronyms[acronym] = 1
        spoken = sr_interface.spoken_acronym(acronym)
        self._add_corresponding_expansion(acronym, spoken)
        return spoken

    def preferred_abbreviations(self, word):
        """returns the preferred abbreviations for the given word

        **INPUTS**

        *STR word* -- the original word

        **OUTPUTS**

        *[STR]* -- list of abbreviations
        """
        try:
            return self.abbreviations[word]
        except KeyError:
            return [word]

    def first_letter(self, symbol):
        """finds the first letter (alphabetic only) of a symbol

        **INPUTS**

        *STR symbol* -- the written form of the symbol

        **OUTPUTS**

        *STR* -- the first letter
        """
        m = re.search('[a-zA-Z]', symbol)
        if m:
            return m.group().lower()
        return None

    def symbols_as_one_string(self, first_letter):
        """Returns a string that lists all the native known symbols
        starting with a given letter.

        This string is used for matching pseudo-symbols to known
        native symbols, because it's much faster than looping through
        keys of *symbol_info*.

        To avoid regenerating this string everytime, the last value
        returned is cached in [self._cached_symbols_as_one_string]
        
        **INPUTS**
        
        *none* -- 
        
        **OUTPUTS**
        
        *STR* -- a string that lists all the native known symbols
        starting with a given letter, with two spaces in between each
        symbol.

        .. [self._cached_symbols_as_one_string] file:///./SymDict.SymDict.html
        """
        first_letter = first_letter[0].lower()
        if not first_letter.isalpha():
            return ""

        if not self._cached_symbols_as_one_string.has_key(first_letter):
            #
            # Cached value has become stale. Regenerate it.
            #
            s = ''
            if not self._symbols_starting_with.has_key(first_letter):
                self._symbols_starting_with[first_letter] = {}
            symbol_list = self._symbols_starting_with[first_letter].keys()
            symbol_list.sort()    
            symbol_list.append('')
            #
            # Note: symbols must be separated by two spaces because
            #       re.findall only returns non-overlapping matches.
            #       But since the regexp used for symbol matching requires
            #       that there be a space before and after the symbol,
            #       if two matching symbols were consecutive in
            #       _cached_symbols_as_one_string, and were separated by
            #       a single space, only the first symbol would be matched
            #
            self._cached_symbols_as_one_string[first_letter] = \
                ' ' + string.join(symbol_list, '  ')


        return self._cached_symbols_as_one_string[first_letter]

    def print_symbols(self, symbols = None):
        """Print the content of the symbols dictionary.
        
        **INPUTS**

        *[STR] symbols* -- list of symbols to print, or None to print
        the whole dictionary
        
        **OUTPUTS**
        
        *none* -- 
        """
        trace("SymDict.print_symbols", "** invoked")
        if symbols:
            the_symbols = symbols
        else:
            the_symbols = self.symbol_info.keys()
            the_symbols.sort()        
        for a_symbol in the_symbols:
            try:
                a_symbol_info = self.symbol_info[a_symbol]
                forms = copy.copy(a_symbol_info.spoken_forms)
                forms.sort()
                print '%s: %s' % (a_symbol, str(forms))
            except KeyError:
                print '%s: undefined' % a_symbol

        if symbols is None:
            letters = self._cached_symbols_as_one_string.keys()
            letters.sort()
            for letter in letters:
                #
                # Note: For external printing purposes (regression testing), force the 
                # regeneration of the symbol string, to make sure it's always
                # sorted in the same way.
                #
                del self._cached_symbols_as_one_string[letter]
                trace("SymDict.print_symbols", "** letter=%s, self.symbols_as_one_string(letter)=%s"  % (letter, self.symbols_as_one_string(letter)))
                print '_cached_symbols_as_one_string[%s] is:\n   %s' \
                    % (letter, self.symbols_as_one_string(letter))

    def print_abbreviations(self, show_unresolved=0):
        """Prints the known and unresolved abbreviations."""

        print 'List of abbreviations\n'
        sorted_abbreviations = self.expansions.keys()
        sorted_abbreviations.sort()
        for an_abbreviation in sorted_abbreviations:
            sorted_expansions = copy.copy(self.expansions[an_abbreviation])
            sorted_expansions.sort()
            print '\'%s\' expands to %s' % (an_abbreviation, sorted_expansions)

        if show_unresolved:
            print '\n\nList of unresolved abbreviations\n'
            sorted_unresolved = self.unresolved_abbreviations.keys()
            sorted_unresolved.sort()
#            sorted_unresolved.sort(lambda x, y: len(x) > len(y) or (len(x) == len(y) and x < y))
            for an_abbreviation in sorted_unresolved:
                symbol_list = self.unresolved_abbreviations[an_abbreviation].keys()
                symbol_list.sort()
                print '\'%s\': appears in %s' % (an_abbreviation, str(symbol_list))


    def peek_at_unresolved(self):
        """returns a reference to the dictionary of unresolved abbreviations
        and the symbols containing those abbreviations.

        **NOTE:** This method is intended only for diagnostic testing
        purpose.  The caller must not modify the dictionary returned

        **INPUTS**

        *none*

        **OUTPUTS**

        *{STR: {STR: 1}}* unresolved_abbreviations={} -- Dictionary of
        unresolved abbreviations. These are abbreviations that have
        appeared in at least one compiled symbol, yet are neither a word
        in the speech vocabulary or a known abbreviation. Values are
        dictionnaries that list the symbols containing the unresolved
        abbreviation.
        """
        return self.unresolved_abbreviations

    def changed_since_sym_file(self, path):
        """indicates whether the file with the given path has changed
        since the last change to the persistent symbol dictionary file
        from which this SymDict was restored. 

        **INPUTS**

        *STR path* -- the path to the file to check

        **OUTPUTS**

        *BOOL* -- true if the file has changed more recently than the
        persistent symbol dictionary file, or if this SymDict instance
        was not initialized from a persisten symbol dictionary
        """
        trace('SymDict.changed_since_sym_file', 
           'symdict file modified %s' % self.file_time)
        trace('SymDict.changed_since_sym_file', 
           'file %s modified %s' % (path, util.last_mod(path)))
        if self.from_file and util.last_mod(path) <= self.file_time:
            return 0
        return 1

    def abbrev_config(self):
        """import abbreviations from standard files
        
        **INPUTS**
        
        *none* 
        
        **OUTPUTS**
        
        *none* 
        """
        existing = []
        for file in self.abbrev_sources:
            if os.path.exists(file):
                existing.append(file)

        if self.from_file:
            files = []
            for file in existing:
                if self.changed_since_sym_file(file):
                    files.append(file)
        else:
            files = existing

        if not files:
            return
        for file in files:
            try:
                try:
                    f = open(file, "r")
                except:
                    msg = 'Error opening abbreviation file:\n%s\n' % file
                    sys.stderr.write(msg)
                    continue
                self.import_abbreviations(f)
            except:
                traceback.print_exc(file = sys.stderr)
    
    def clear_abbreviations(self):
        """remove all existing abbreviations and expansions

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.abbreviations = {}
        self.alt_abbreviations = {}
        self.expansions = {}
        self.pronunciations = {}
        self.acronyms = {}
        self.extra_expansions = {}

    def import_abbreviations(self, f, instructions = None):
        """import abbreviations and expansions from standard files

        Note: import_abbreviations may raise exceptions due to problems
        opening the file or executing the python commands therein

        **INPUTS**

        *file or STR f* -- string or open file object from which to import
        abbreviations

        *STR instructions* -- indicates whether import_abbreviations
        should 'append' the new abbreviations to the existing ones,
        'prepend' them, or 'replace' them.  The default is to 'append'.
        The other options should only be used when the user has
        explicitly requested them, and (for 'replace') confirmed that
        existing abbreviations will be lost.

        **OUTPUTS**

        *none*

        """
        names = {}
        names['add_abbreviation'] = self.add_abbreviation
        names['add_abbreviations'] = self.add_abbreviations
        names['add_alt_abbreviations'] = self.add_alt_abbreviations
        names['add_acronym'] = self.add_acronym
        names['add_pronunciation'] = self.add_pronunciation
        names['add_extra_expansion'] = self.add_extra_expansion
        if instructions == 'prepend':
# store current abbreviations
            temp_storage = StringIO.StringIO()
            self.export_to_file(temp_storage)
        if instructions == 'prepend' or instructions == 'replace':
            self.clear_abbreviations()
        exec f in names
        if instructions == 'prepend':
# append back the old abbreviations
            self.import_abbreviations(temp_storage)
    
    def import_recent(self):
        """recover abbreviations and expansions from most recent export
        file

        Note: import_abbreviations (called by import_recent)
        may raise exceptions due to problems opening the file or 
        executing the python commands therein

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the name of the file from which abbreviation preferences 
        were successfully recovered, or None if they were not
        """
        on_exit = self.export_file + '.py'
        on_init = self.export_file + '.on_init.py'
        recent = None
        exit_date = None
        if os.path.exists(on_exit):
            exit_date = util.last_mod(on_exit)
            recent = on_exit
        if os.path.exists(on_init):
            init_date = util.last_mod(on_init)
            if recent and init_date > exit_date:
                recent = on_init
        if not recent:
            return None
        try:
            f = open(recent, "r")
            self.import_abbreviations(f)
            return recent
        except:
            traceback.print_exc(file = sys.stderr)
            self.clear_abbreviations()
            return None

    def finish_config(self, mark_user = 1):
        """Finish performing those parts of the SymDict
        configuration which can't take place until after the VoiceCode
        configuration files have been executed
        
        **INPUTS**

        *BOOL mark_user* -- if true, mark the user so we don't have to
        scan the symbol files every time.  Generally, this should be
        true except in the regression test for SymDict persistence

        *none*
        
        **OUTPUTS**
        
        *none*
        """
        self.abbrev_config()
        self.maybe_parse_standard_symbols()
# mark this user so we don't have to scan the symbol files every time
        sr_interface.mark_user()

    def maybe_parse_standard_symbols(self):
        """Parse standard symbols for the various programming languages,
        but only if this is a fresh VoiceCode user, or if the files 
        have changed more recently than the persistent symbol dictionary
        file

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        existing = []
        for file in self.standard_symbol_sources:
            if os.path.exists(file):
                existing.append(file)

# files no longer in standard_symbol_sources should be eliminated from
# the list of files read, because the symbols read from them may no 
# longer be up to date
        files_read = []
        for file in self.symbol_sources_read:
            if file in self.standard_symbol_sources:
                files_read.append(file)
        self.symbol_sources_read = files_read

#        print 'existing files:', existing
#        print 'from_file = ', self.from_file
#        print 'marked = ', sr_interface.is_user_marked()
        debug.trace('SymDict.maybe_parse_standard_symbols', 
            'from_file = %d' % self.from_file)
        debug.trace('SymDict.maybe_parse_standard_symbols', 
            'standard symbol sources: %s' % self.standard_symbol_sources)
        debug.trace('SymDict.maybe_parse_standard_symbols', 
            'symbol sources already read: %s' % self.symbol_sources_read)
        if self.from_file and sr_interface.is_user_marked():
            files = []
            for file in existing:
                debug.trace('SymDict.maybe_parse_standard_symbols', 
                    'checking file %s' % file)
# need to scan files which have just been added to the standard symbols
# sources list, and re-scan files changed since the persistent SymDict file 
# was saved
                if self.changed_since_sym_file(file) or \
                    file not in self.symbol_sources_read:
                    files.append(file)
#            print 'changed files:', files
        else:
            files = existing
#            print 'all existing files:', files

        if files:
            self.parse_symbols_from_files(files)

    def parse_standard_symbols(self):
        """Parse standard symbols for the various programming languages.
        
        **INPUTS**
        
        *none* 
        
        **OUTPUTS**
        
        *none* 
        """
        self.parse_symbols_from_files(self.standard_symbol_sources)
    
    def parse_symbols_from_files(self, file_list, add_sr_entries=1):
        """Parse symbols from a series of source files

        **INPUTS**

        *[STR] file_list -- List of files to be compiled

        *BOOL* add_sr_entries = 1 -- If true, add symbols to the SR vocabulary

        **OUTPUT**

        *none* --
        """

        for a_file in file_list:
            print 'Compiling symbols for file \'%s\'' \
                % util.within_VCode(a_file)
            self.parse_symbols_from_file(a_file, add_sr_entries=add_sr_entries)
# add to list of files already scanned and up to date
            if a_file in self.standard_symbol_sources and \
                a_file not in self.symbol_sources_read:
                self.symbol_sources_read.append(a_file)
            
        #
        # Save dictionary to file
        #
        self.save()
                

    def parse_symbols_from_file(self, file_name, add_sr_entries=1):
        """Parse symbols from a source file.

        *STR* file_name -- The path of the file.

        *BOOL* add_sr_entries = 1 -- If true, add symbols to the SR vocabulary
        
        Parsed symbols are stored in the *symbol_info* and
        *spoken_forms2symbol* attributes.
        """

#        print '-- SymDict.parse_symbols: file_name=%s' % file_name
        try:
            source_file = open(file_name, 'r')
        except Exception:
            #
            # If the file doesn't exist, we just do nothing
            #
            print 'WARNING: source file \'%s\' doesn\'t exist.' % util.within_VCode(file_name)
        else:
            source = source_file.read()
            
#            print '-- SymDict.parse_symbols: \n*** START OF SOURCE ***\n%s\n*** END OF SOURCE ***' % source

            language_name = self.get_language_by_filename(file_name)
#            print '-- SymDict.parse_symbols: language_name=%s' % language_name
            self.parse_symbols(source, language_name, 
                add_sr_entries = add_sr_entries)
                
    def parse_symbols(self, contents, language_name, add_sr_entries=1):
        """Parse symbols from a string representing the contents of a 
        source file.

        *STR* contents -- the contents of the source file

        *STR* language_name -- the name of the language of the source
        file

        *BOOL* add_sr_entries = 1 -- If true, add symbols to the SR vocabulary
        
        Parsed symbols are stored in the *symbol_info* and
        *spoken_forms2symbol* attributes.
        """


        language_definition = self.get_language_definition(language_name)
        debug.trace('SymDict.parse_symbols', 'language_name=%s, language_definition=%s' % (language_name, language_definition))
        stripped_contents = self.strip_source(contents, language_definition)

            #
            # Parse symbols from the first chunk
            #
        while stripped_contents != '':
            a_match = re.search('(' + \
                language_definition.regexp_symbol + ')', stripped_contents)
            if a_match:
                self.add_symbol(a_match.group(1), 
                    add_sr_entries=add_sr_entries)
                stripped_contents = stripped_contents[a_match.end()+1:]
            else:
                stripped_contents = ''
                
    def strip_source(self, source, language_definition):
        """Removes all parts of a source file that don't contain symbols.

        This includes comments and quoted strings.
        
        **INPUTS**
        
        *STR* source -- the source

        *BOOL* add_sr_entries = 1 -- If true, add symbols to the SR vocabulary.

        [LangDef] language_definition -- the definition of the
        language that *source* is written in.
        

        **OUTPUTS**
        
        *STR* stripped_source -- source stripped of all non-symbols chunks

        .. [LangDef] file:///./LangDef.LangDef.html"""

#        print '-- SymDict.strip_source: source=\n%s\n*** End of source' % source
        stripped_source = ''
        while source != '':
            #
            # Identify earliest chunk of code that doesn't contain any symbols
            #
            non_symbol_start = None
            non_symbol_end = None
            for a_regexp in language_definition.regexps_no_symbols:
#                print '-- SymDict.strip_source: trying to strip regexp: %s ' % a_regexp
                a_match = re.search(a_regexp, source)
                if a_match:
#                    print '-- SymDict.strip_source: we have a match for that regexp'
                    if non_symbol_start == None or a_match.start() < non_symbol_start:
                        non_symbol_start = a_match.start()
                        non_symbol_end = a_match.end()
#                        print '-- SymDict.strip_source: updated non_symbol_start=%s, non_symbol_end=%s' % (non_symbol_start, non_symbol_end)                        

            #
            # Rip that part of the code out
            #
            if non_symbol_start != None:
#                print '-- SymDict.strip_source: stripping following text\n%s\n*** End of stripped text' % source[non_symbol_start:non_symbol_end]                
                stripped_source = stripped_source + source[0:non_symbol_start]
                source = source[non_symbol_end:]
            else:
                stripped_source = stripped_source + source
                source = ''
                
        return stripped_source

    def clear_standard_symbols_file_list(self):
        """Clears the list of files defining standard symbols"""
        self.standard_symbol_sources = []                                

    def standard_symbols_in(self, file_list):
        """Specify source files defining standard symbols"""
        debug.trace('SymDict.standard_symbols_in', "file_list=%s" % file_list)
        for a_file in file_list:
            if not a_file in self.standard_symbol_sources:
                self.standard_symbol_sources.append(a_file)

    def abbreviations_in(self, file_list):
        """Specify source files defining expansions and abbreviations"""
        for a_file in file_list:
            if not a_file in self.abbrev_sources:
                self.abbrev_sources.append(a_file)

    def symbol_is_tentative(self, symbol):
        """Indicates whether or not a symbol is tentative and can be
        removed as a consequence of a correction.

        **INPUTS**

        *STR* symbol -- native symbol to remove

        **OUTPUTS**

        *BOOL* -- true if the symbol was only tentative.
        """
        is_tentative = 0
        if self.tentative_symbols.has_key(symbol) and self.tentative_symbols[symbol]:
           is_tentative = 1
        return is_tentative
    

    def remove_symbol_if_tentative(self, symbol):
        """remove a symbol which was tentatively added
        from the dictionary

        **INPUTS**

        *STR* symbol -- native symbol to remove

        **OUTPUTS**

        *BOOL* -- true if the symbol was only tentative, and 
        was successfully removed
        """
        debug.trace('SymDict.remove_symbol_if_tentative', 
            'symbol = %s' % symbol)
        success = 0
        if self.symbol_is_tentative(symbol):
            debug.trace('SymDict.remove_symbol_if_tentative', 
                'is tentative')
            success = self.remove_symbol(symbol)
        debug.trace('SymDict.remove_symbol_if_tentative', 
            'success = %d' % success)
        return success

    def remove_symbol(self, symbol, remove_sr_entries = 1):
        """remove a symbol from the dictionary

        **INPUTS**

        *STR* symbol -- native symbol to remove

        *BOOL* remove_sr_entries = 1 -- If true, remove the corresponding
        entries from the SR vocabulary.

        **OUTPUTS**

        *BOOL* -- true if the symbol was known and was successfully
        removed
        """
        debug.trace('SymDict.remove_symbol', 
            'symbol = %s' % symbol)
        try:
            symbol_info = self.symbol_info[symbol]
        except KeyError:
            debug.trace('SymDict.remove_symbol', 
                'unknown symbol')
            return 0

        spoken_forms = symbol_info.spoken_forms
        for spoken_form in spoken_forms:
            phrase = self.spoken_to_phrase(spoken_form)
            debug.trace('SymDict.remove_symbol', 
                'removing spoken forms: phrase = \n%s' % phrase)
            symbol_list = self.spoken_form_info.complete_match(phrase)
            debug.trace('SymDict.remove_symbol', 
                'complete match returned %s' % symbol_list)
            if remove_sr_entries:
                self.remove_vocabulary_entry(symbol, spoken_form)
            if not (symbol_list is None):
                try:
                    symbol_list.remove(symbol)
                    debug.trace('SymDict.remove_symbol', 
                        'symbol_list now %s' % repr(symbol_list))
                    if not symbol_list:
                        remove_branch = \
                            self.spoken_form_info.remove_phrase(phrase)
                        if remove_branch is None:
                            debug.trace('SymDict.remove_symbol', 
                                'failed to remove empty branch')
                except ValueError:
                    debug.trace('SymDict.remove_symbol', 
                        'symbol not found in symbol_list')
                    pass

        del self.symbol_info[symbol]
        if self.tentative_symbols.has_key(symbol):
            del self.tentative_symbols[symbol]

        letter = self.first_letter(symbol)
        del self._symbols_starting_with[letter][symbol]

        if self._cached_symbols_as_one_string.has_key(letter):
            del self._cached_symbols_as_one_string[letter]
        return 1

    def _add_symbol_starting_with(self, symbol):
        """private method which adds a symbol to the dictionary of
        symbols starting with a given letter

        **INPUTS**

        *STR symbol* -- the written form of the symbol

        **OUTPUTS**

        *STR* -- the first letter of the symbol
        """
        first_letter = self.first_letter(symbol)
        try:
            common = self._symbols_starting_with[first_letter]
        except KeyError:
            common = {}
            self._symbols_starting_with[first_letter] = common
        common[symbol] = 1
        return first_letter


    def add_symbol(self, symbol, user_supplied_spoken_forms=[], 
                   tentative = 1, add_sr_entries=1):
        """Add a symbol to the dictionary

        **INPUTS**
        
        *STR* symbol -- Symbol to add

        *[STR] user_supplied_spoken_forms* -- Spoken forms for the
         symbol which were supplied explicitly by the user. These
         forms are added even if they are not generated automatically by
         [get_spoken_forms]. This is useful in cases where the user
         has explicitly supplied spoken forms for a symbol that contains very
         short abbreviations (i.e. abbreviations that are rejected by
         [add_abbreviation]). In such cases, the spoken form wouldn't
         automaticaly be generated by [updated_spoken_forms] and must
         therefore be added explictely by add_symbol.

        *BOOL* tentative = 1 -- If true, symbol is added tentatively,
        and can be removed on undo/correction/reformatting

        *BOOL* add_sr_entries = 1 -- If true, adds symbol to the SR vocabulary.

        
        **OUTPUTS**
        
        *none* -- 

        .. [get_spoken_forms] file:///./SymDict.SymDict.html#get_spoken_forms
        .. [add_abbreviation] file:///./SymDict.SymDict.html#add_abbreviation"""
        
        if tracing('SymDict.add_symbol'):
            trace('SymDict.add_symbol', 'symbol=%s' % symbol)

        spoken_forms = user_supplied_spoken_forms[:]
        
        if not self.symbol_info.has_key(symbol):

            #
            # Add an entry to the symbol dictionary
            #
            self.symbol_info[symbol] = SymbolInfo(spoken_forms)
            
            self.tentative_symbols[symbol] = tentative

            if tracing('SymDict.add_symbol'):
                trace('SymDict.add_symbol', 'new symbol=%s' % symbol)

            #
            # Add the symbol to the string used for symbol matching
            #

            new_string_entry = ' %s ' % symbol

            letter = self._add_symbol_starting_with(symbol)

            if not self._cached_symbols_as_one_string.has_key(letter):
                self._cached_symbols_as_one_string[letter] = new_string_entry
            else:
                self._cached_symbols_as_one_string[letter] = \
                    self._cached_symbols_as_one_string[letter] + \
                    new_string_entry

            generated_forms = self.get_spoken_forms(symbol)
            spoken_forms.extend(generated_forms)

            self.symbol_info[symbol].add_spoken_forms(generated_forms)

        else:
            #
            # Add user supplied spoken forms
            #
            self.symbol_info[symbol].add_spoken_forms(spoken_forms)
            
        if add_sr_entries:
            for form in spoken_forms:
                added = self.add_vocabulary_entry(symbol, form)
                # for user-specified forms, and acronyms, we want to use the
                # capitalization and punctuation given for the vocabulary entry

        self._update_spoken_form_info(symbol, spoken_forms)
           

    def _update_spoken_form_info(self, symbol, spoken_forms):
        """
        Add new spoken forms to the spoken_form_info data structure

        NOTE: this method does not add spoken_forms to the symbol info
        object

        ** INPUTS **

        *STR symbol* -- the written form of the symbol

        *[STR] spoken_forms* -- the list of spoken_forms to add
        """

        for a_form in spoken_forms:
            # convert spoken form to searchable form
            # before adding to the lookup Trie 
            phrase = self.spoken_to_phrase(a_form)
            symbol_list = self.spoken_form_info.complete_match(phrase)
            if symbol_list is None:
                symbol_list = [symbol]
            elif symbol not in symbol_list:
                symbol_list.append(symbol)
            self.spoken_form_info.add_phrase(phrase, symbol_list)

    def spoken_to_phrase(self, spoken_form):
        """converts a spoken form as added to the speech engine's
        vocabulary to the phrase used in spoken_form_info

        **INPUTS**

        *STR* spoken_form -- the spoken form of the vocabulary entry

        **OUTPUTS**

        *[STR]* -- the corresponding phrase to add to spoken_form_info
        """
        clean_form = re.sub('[^a-zA-Z0-9 ]', ' ', spoken_form)
        clean_form = clean_form.strip()
        clean_form = string.lower(clean_form)
        phrase = string.split(clean_form)
        return phrase

    def add_vocabulary_entry(self, symbol, spoken_form):
        """adds a vocabulary entry corresponding to a spoken form for a
        symbol

        **INPUTS**

        *STR* symbol -- the written form of the native symbol 
        
        *STR* spoken_form -- its spoken form

        **OUTPUTS**

        *BOOL* -- true if the word was added
        """
        #
        # Add spoken form to NatSpeak's vocab if not already there.
        #
        # Note: We want to avoid adding single words with identical
        # written and spoken forms to the vocabulary, because they
        # might be unresolved abbreviations.
        #
        # Since we don't remove words on exit, 
        # such words would be indistinguishable from
        # words that have been added by the real NatSpeak
        # Vocabulary Builder.
        #
        # So the next time we start VoiceCode and compile a
        # symbol that contains that abbreviation, the
        # abbreviation would not be logged as unresolved
        # (because it would correspond to an in-vocabulary word)
        #
        entry = None
        if vocabulary_symbols_with_written_form:
            #
            # Add the vocabulary entry as a written\\spoken
            # form
            #

            # this condition guards against adding unknown single words
            if spoken_form != symbol:
                entry = sr_interface.vocabulary_entry(spoken_form, symbol)
        else:
            #
            # Add just the spoken form entry
            #
            # since we are adding only the spoken form, the
            # condition is stricter
            if len(re.split('\s+', spoken_form)) > 1:
                entry = sr_interface.vocabulary_entry(spoken_form)
                
        if entry: 
            added = sr_interface.addWord(entry)        
            if added:
                return 1

        return 0
        
    def correct_symbol(self, spoken_form_used, bad_written_form, correct_written_form):
        """Correct the written form of a symbol.

        **INPUTS**
        
        *STR* spoken_form_used -- Spoken form of the symbol.

        *STR bad_written_form* -- written form that was matched incorrectly to
        *spoken_form*.
        
        *STR correct_written_form* -- written form that SHOULLD have been used instead
        for *spoken_form*.
        
        **OUTPUTS**
        
        *none* -- 
        
        **SIDE EFFECTS**
        
        If *bad_written_form* is a new symbol that was created tentatively on account
        of *spoken_form* beind said, then this method will remove *bad_written_form*
        from the dictionnary altogether. Otherwise, it will simply change the 
        priority of *bad_written_form* and *correct_written_form* for
        *spoken_form*.        
        """
        debug.trace('SymDict.correct_symbol', 
                    'spoken_form_used=%s, bad_written_form=%s, correct_written_form=%s' % 
                    (spoken_form_used, bad_written_form, correct_written_form))
        self.move_written_form_to_top_of_priority_list(spoken_form_used, 
                                                       correct_written_form)
        self.add_symbol(correct_written_form, [spoken_form_used], tentative=0)
        self.save()

    def move_written_form_to_top_of_priority_list(self, spoken_form, written_form):
        """Moves a written form to the top of the priority list for a spoken form
        
        **INPUTS**
        
        *STR* spoken_form* -- Spoken form for which we want to change the priority list of
        written forms.

        *STR written_form* -- Written form that we want to bump to the head of the priority list.
        
        **OUTPUTS**
        
        *none* -- 
        
        """
        debug.trace('SymDict.move_written_form_to_top_of_priority_list', 
                    'spoken_form=%s, written_form=%s, string.split(spoken_form)=%s' % (spoken_form, written_form, repr(string.split(spoken_form))))
        written_forms_priority_list = self.match_head(string.split(spoken_form))[0]
        debug.trace('SymDict.move_written_form_to_top_of_priority_list', 
                    "written_forms_priority_list=%s" % repr(written_forms_priority_list))
        if (written_forms_priority_list == None):
           written_forms_priority_list = []
        written_forms_priority_list = \
           util.remove_occurences_from_list(written_form, written_forms_priority_list)
        written_forms_priority_list = [written_form] + written_forms_priority_list
        self.spoken_form_info.add_phrase(string.split(spoken_form), written_forms_priority_list)


    def remove_vocabulary_entry(self, symbol, spoken_form):
        """removes a vocabulary entry corresponding to a spoken form for a
        symbol which is being removed

        **INPUTS**

        *STR* symbol -- the written form of the native symbol 
        
        *STR* spoken_form -- its spoken form

        **OUTPUTS**

        *none*
        """
        entry = None
        if vocabulary_symbols_with_written_form:
            #
            # Add the vocabulary entry as a written\\spoken
            # form
            #

            # this condition guards against adding unknown single words
            if spoken_form != symbol:
                entry = sr_interface.vocabulary_entry(spoken_form, symbol)
        else:
            #
            # Add just the spoken form entry
            #
            # since we are adding only the spoken form, the
            # condition is stricter
            if len(re.split('\s+', spoken_form)) > 1:
                entry = sr_interface.vocabulary_entry(spoken_form)
                
        if entry: sr_interface.deleteWord(entry)        


    def get_spoken_forms(self, symbol):
        """Returns a list of possible spoken forms for a symbol.
        
        **INPUTS**
        
        *STR* symbol -- the symbol in question 
        

        **OUTPUTS**
        
        *[STR]* -- returns a list of spoken forms
        """

#        print '-- SymDict.get_spoken_forms: symbol=%s, abbreviations:' % symbol; self.print_abbreviations()

        
        #
        # First, split the symbol into words or abbreviations
        #
        mod_symbol = symbol
               # Replace non alphanums by space
        mod_symbol = re.sub('[^a-zA-Z0-9]+', ' ', mod_symbol)
#        print '-- SymDict.get_spoken: after 1st sub, mod_symbol=\'%s\'' % mod_symbol
               # Split before and after string of numbers
        mod_symbol = re.sub('([0-9]+)', ' \\1 ', mod_symbol)

               # Split when there is a change of case. Must distinguish between
               # the following cases:
               #    'XXXyyy'   -> 'XX Xyyy'
               #    'xxxYYY'   -> 'xxx YYY'               
        mod_symbol = re.sub('([A-Z]+?)([A-Z][a-z]+)', '\\1 \\2', mod_symbol)
#        print '-- SymDict.get_spoken: after 1st sub, mod_symbol=\'%s\'' % mod_symbol                
        mod_symbol = re.sub('([a-z]+)([A-Z]+)', '\\1 \\2', mod_symbol)
#        print '-- SymDict.get_spoken: after 2nd sub, mod_symbol=\'%s\'' % mod_symbol        
               # Remove leading/trailing spaces
        mod_symbol = re.sub('(^\s+|\s+$)', '', mod_symbol)
#        print '-- SymDict.get_spoken: after 3rd sub, mod_symbol=\'%s\'' % mod_symbol
        mod_symbol = string.lower(mod_symbol)
        words = re.split('\s+', mod_symbol)
        

#        print '-- SymDict.get_spoken: mod_symbol=\'%s\', words=%s' % (mod_symbol, str(words))
        #
        # Replace each abbreviated word by its possible expansion
        #
        possibilities = []
        for a_word in words:
            possibilities = possibilities + [self.expand_word(a_word, symbol)]
#        print '-- SymDict.get_spoken_forms: possibilities=%s' % possibilities

        #
        # Generate all possible spoken forms for that symbol
        #
        
        the_spoken_forms = self.expand_possible_forms([''], possibilities)
                                                      
        # don't add unknown words to the vocabulary, otherwise next
        # time we won't be able to tell that they are unknown
        
# add_vocabulary_entry now filters for this

#        the_spoken_forms = filter(lambda form: form != symbol,
#                                  the_spoken_forms)
            
#        print '-- SymDict.get_spoken_forms: the_spoken_forms=%s' % the_spoken_forms        
        return the_spoken_forms


    def expand_word(self, word, symbol):
        """Expands a word from a symbol to its possible spoken forms.

        If *word* is an in-vocabulary word simply returns *[word]*

        If it's a known abbreviation returns the list of possible expansions
        for that abbreviation or the list of their plural forms.

        If it's the plural of a known abbreviation, returns the plural
        form of the abbreviation's expansions.

        Otherwise, it tries to split *word* into substrings that are
        in-vocabulary words or known abbreviations (*NOT IMPLEMENTED
        AT THE MOMENT).

        If that doesn't work either logs *word* in
        *self.unresolved_abbreviations* and log *symbol* as one of the symbol
        where it occured.
        
        **INPUTS**
        
        *STR* abbreviation -- Abbreviation to be expanded 
        
        *STR* symbol -- Symbol in which the word appeared

        **OUTPUTS**
        
        *[STR] expansions * -- list of possible expansions of the word.
        """
        
#        print '-- SymDict.expand_word: expanding word: \'%s\'' % word

        #
        # Check if word might be a pluralised word
        #
        word_length = len(word)
        if word_length > 1 and word[-1] == 's':
            single_form = word[0:-1]
        else:
            single_form = word
        
        expansions = [word]
        if self.expansions.has_key(word):
            #
            # word is a known abbreviation. Add expansions.
            #
            expansions = expansions + self.expansions[word]            
        elif self.expansions.has_key(single_form):
            #
            # word is the plural of an abbreviation
            #
            expansions = map(lambda an_expansion: pluralize(an_expansion), 
                self.expansions[single_form])
        else:
            #
            # Word is not a known abbreviation nor plural of a known
            # abbreviation
            # Check if this is an unresolved abbreviation
            # (note: flag=4 means case unsensitive)
            #
            expansions = self.check_word(word, symbol)

#        print '-- SymDict.expand_word: returning expansions=%s' % expansions
        expansions.sort()
        return expansions

    def std_word_exists(self, spoken_form, written_form = None):
        """
        Checks whether the given word exists in the speech-recognition
        engine

        ** INPUTS **

        *STR spoken_form* -- the spoken_form of the word

        *STR written_form* -- the written_form of the word

        ** OUTPUTS **

        *BOOL* -- true if the word exists
        """
        entry = sr_interface.vocabulary_entry(spoken_form, written_form)
        return not (sr_interface.getWordInfo(entry) is None)

    def check_word(self, word, symbol):
        """Finds pronunciation expansions of a word, based on whether or
        not it is in the vocabulary
        based on .

        **INPUTS**
        
        *STR* word -- word to check
        
        *STR* symbol -- Symbol in which the word appeared

        **OUTPUTS**

        *[STR] expansions * -- list of possible expansions of the word.
        """
        word_exists = self.word_exists
        if not word_exists:
            word_exists = self.std_word_exists

# some versions of natspeak define "non", etc. without hyphens.  For
# consistency, we prefer the hyphenated versions for common
# hyphenated words
        hyphenated = word + '-'
        hyphenated_pron = \
            sr_interface.vocabulary_entry(word, hyphenated)
        if self.common_hyphenated.has_key(word):
            return [hyphenated]
        if word_exists(word):
            expansions = [word]
            if len(word) == 1 and word[0].isalpha():
                acronym = string.lower(word)
                spoken = sr_interface.spoken_acronym(acronym)
                expansions.append(spoken)
            return expansions
        if word_exists(word, hyphenated):
            return [hyphenated]
        if len(word) > 1:
            capped =  word.capitalize()
# if capitalized form exists, assume that the word is pronouncable (even
# without the capitalization)
            if word_exists(capped):
                return [word]
        upper = word.upper()
        if word_exists(upper):
# if word exists only in all-caps form, assume that the effective
# pronunciation is as an acronym, which may not be pronouncable if not
# all caps
            expansions = [upper]
            if len(word) <= self.max_auto_acronym and word.isalpha():
                acronym = string.lower(word)
                spoken = sr_interface.spoken_acronym(acronym)
                if len(word) == 1:
                    expansions = [spoken]
                else:
                    expansions.append(spoken)
            return expansions
        if self.unresolved_abbreviations.has_key(word):
            self.unresolved_abbreviations[word][symbol] = 1
        else:
            self.unresolved_abbreviations[word] = {symbol: 1}
        expansions = [word]
        if len(word) <= self.max_auto_acronym and word.isalpha():
            acronym = string.lower(word)
            spoken = sr_interface.spoken_acronym(acronym)
            if len(word) == 1:
                expansions = [spoken]
            else:
                expansions.append(spoken)
        return expansions
            
    def expand_possible_forms(self, partial_forms, further_extensions):
        """Returns a list of possible spoken forms for a symbol.
        
        **INPUTS**
        
        *[STR]* partial_forms -- a list of partially completed spoken
         forms. 

        *[[STR]]* further_extensions -- a list of possibilities for
         further extending the spoken forms in *partal_forms*.
        

        **OUTPUTS**
        
        *[STR]* -- List of all possible spoken forms for the symbol.
        
        """
#        print '-- SymDict.expand_possible_forms: partial_forms=%s, further_extensions=%s' % (partial_forms, further_extensions)
        if len(further_extensions) == 0:
            return partial_forms
        else:            
            #
            # There are more possible extensions
            #
            # Extend each partial expansion by one level, then call
            # expand_possible_forms recursively
            #
            next_extensions = further_extensions[0]
            if len(further_extensions) > 1:
                further_extensions = further_extensions[1:]
            else:
                further_extensions = []

#            print '-- SymDict.expand_possible_forms: next_extensions=%s' % next_extensions
            extended_partial_forms = []            
            for a_partial_form in partial_forms:
                for an_extension in next_extensions:
#                    print '-- SymDict.expand_possible_forms: a_partial_form=%s, an_extension=%s' % (a_partial_form, an_extension)
                    if a_partial_form == '':
                        an_extended_partial_form = an_extension
                    elif a_partial_form[-1] == '-':
                        an_extended_partial_form = a_partial_form + an_extension
                    else:
                        an_extended_partial_form = a_partial_form + ' ' + an_extension
                    extended_partial_forms = extended_partial_forms + [an_extended_partial_form]

#            print '-- SymDict.expand_possible_forms: further_extensions=%s' % further_extensions
            expanded_forms = self.expand_possible_forms(extended_partial_forms , further_extensions)
                    
            return expanded_forms
            
    def get_language_by_filename(self, file_name):
        """Gets the name of the language associated with a source file.
        
        **INPUTS**
        
        *STR* file_name -- name of the file 
        

        **OUTPUTS**
        
        *STR* -- name of the language.  Returns *None* if there doesn'
        t exist a proper language definition, or if can't tell what 
        language the source file is written in.

        """

        language_name = self.lang_name_srv.file_language_name(file_name)
        return language_name

            
    def get_language_definition_by_filename(self, file_name):
        """Gets the definition of the language associated with a source file.
        
        **INPUTS**
        
        *STR* file_name -- name of the file 
        

        **OUTPUTS**
        
        [LangDef] -- definition of the language *file_name* is written
        in. Returns *None* if there doesn't exist a proper language
        definition, or if can't tell what language the source file is
        written in.

        ..[LangDef] file:///./LangDef.LangDef.html"""

        global language_definitions
        definition = None
        
        language_name = self.get_language_by_filename(file_name)
        return self.get_language_definition(language_name)

    def get_language_definition(self, language_name):
        """Gets the definition of the language associated with a source file.
        
        **INPUTS**
        
        *STR* file_name -- name of the file 
        

        **OUTPUTS**
        
        [LangDef] -- definition of the language *file_name* is written
        in. Returns *None* if there doesn't exist a proper language
        definition, or if can't tell what language the source file is
        written in.

        ..[LangDef] file:///./LangDef.LangDef.html"""

        global language_definitions
        definition = None
        
        if tracing('SymDict.get_language_definition'):
            debug.trace('SymDict.get_language_definition', 'language_definitions=%s, language_name=%s' % (language_definitions, language_name))
        if language_definitions.has_key(language_name):
            definition = language_definitions[language_name]
        if tracing('SymDict.get_language_definition'):
            debug.trace('SymDict.get_language_definition', 'returning definition=%s' % definition)            
        return definition
    
    def _score_symbol_matches(self, pseudo_symbol, words, 
                               native_matches):
       """Assign confidence scores to each approximate match to the
       given pseudo_symbol
       
       **INPUTS**

       *STR pseudo_symbol* -- The pseudo symbol that was matched.

       *[STR] words* -- words in the pseudo symbol
       

       *Iterator over Match objects native_matches* -- iterator of
       re.MatchObjects corresponding to native symbol matches for 
       *pseudo_symbol*.
       
       **OUTPUTS**
       
       *(allowed, forbidden)* where

       allowed and forbidden are lists of tuples (score, re.MatchObject)
       each sorted by descending score.  The confidence score is a
       floating point number between 0 and 1 (inclusive).
       Forbidden matches are those for which some rule forbids them
       from being the first choice match (e.g. dropping a final "s").
       Such matches are not given a score of zero, in order to allow
       them to be sorted and displayed in the known symbol match
       dialog.  All other matches are allowed, but the caller should
       still apply a threshhold to the list.  Also, the caller may
       want to re-adjust the scores  according to other
       criteria (such as the source of the native symbol) and re-sort.
       """
       allowed = []
       forbidden = []
       for match in native_matches:
           native_symbol = match.group().strip()
           bad, score = \
               self._score_symbol_match(pseudo_symbol, words, match)
           if bad:
               forbidden.append((score, native_symbol))
           else:
               allowed.append((score, native_symbol))
       allowed.sort()
       allowed.reverse()
       forbidden.sort()
       forbidden.reverse()
       return allowed, forbidden
       
    def _score_symbol_match(self, pseudo_symbol, words, native_match):
       """Assign a confidence score to an approximate match to the
       given pseudo_symbol
       
       **INPUTS**

       *STR pseudo_symbol* -- The pseudo symbol that was matched.

       *[STR] words* -- words in the pseudo symbol

       *re.MatchObject native_match* -- re.MatchObject corresponding
       to native symbol matches for *pseudo_symbol*.
       
       **OUTPUTS**

       *(forbidden, confidence_score)* -- where the confidence score
        is a floating point value between 0 and 1 (inclusive), and
        forbidden is a BOOL indicating whether some rule forbade using
        the match as the first choice (e.g. because a final "s" was
        dropped.)

       Note: The confidence score is not a true statistical confidence, but
       simply a rough gauge which can be used for sorting matches and
       applying a minimum threshold.
       """
       # The final confidence value equals raw_confidence * (1 - doubt)
       # Acceptable word matches reduce the doubt by some factor >= 1,
       # while bad word matches or other forbidden matches decrease the
       # raw_confidence by some factor.  The two separate scales allow for:
       #
       # (a) acceptable but weak matches (e.g. "d" for "directory")
       # which do little to reduce the doubt, but don't increase it
       # either, and
       #
       # (b) forbidden matches which reduce the confidence sharply,
       # even if other words in the symbol are strong matches
       
       raw_confidence = 1.
       doubt = 1.
       forbidden = 0
       word_factor = 2.

       word_groups = native_match.groups('')[1:]
       for word, matched_text in zip(words, word_groups):
           if tracing('SymDict._score_symbol_match'):
               trace('SymDict._score_symbol_match', 
                   'word = %s, matched text = %s' % (word, repr(matched_text)))
           s = self.reg_word_to_native(word, map_letter = lambda x:"(%s)" % x)
           word_match = re.match(s, matched_text, re.IGNORECASE)
           if tracing('SymDict._score_symbol_match'):
               trace('SymDict._score_symbol_match', 
                   'word_match = %s' % word_match)
           factor = self._score_word_match(word, word_match)
           if tracing('SymDict._score_symbol_match'):
               trace('SymDict._score_symbol_match', 
                   'word factor = %f' % factor)
           if factor < 0:
               forbidden = 1
               raw_confidence = raw_confidence * (1. + factor)
               if tracing('SymDict._score_symbol_match'):
                   trace('SymDict._score_symbol_match', 
                       'raw_confidence now = %f' % raw_confidence)
           else:
               doubt = doubt / (1. + factor/word_factor)
               if tracing('SymDict._score_symbol_match'):
                   trace('SymDict._score_symbol_match', 
                       'doubt now = %f' % doubt)

       confidence = raw_confidence * (1. - doubt)
       if tracing('SymDict._score_symbol_match'):
           trace('SymDict._score_symbol_match', 
               'forbidden, confidence = %d, %f' % (forbidden, confidence))
       
       # add code here to modify confidence based on multi-word
       # criteria (e.g. short abbreviations run together, or
       # words run together to match real English words)

       return forbidden, confidence
   
    def _score_word_match(self, word, word_match):
       """Helper method used by _score_symbol_match to see how the
       match for an individual word in the pseudo_symbol modifies the
       confidence in the overall match

       **INPUTS**
    
       *STR word* -- the word being matched

       *re.MatchObject word_match* -- the match for that word
       
       **OUTPUTS**

       *FLOAT conf_mod* -- confidence modifier for the word, with
       -1 < conf_mod <= 1.  If conf_mod < 0, then the confidence will
       be decreased and the match will be considered to be forbidden.
       If it is > 0, then the confidence will be increased.
       """
       whole_match = word_match.group()
       
       if word == whole_match.lower():
           return 1.

       abbrev_score = self._score_abbrev(word, whole_match)
       if abbrev_score:
           return abbrev_score
       
       expansion_score = self._score_expansion(word, 
           whole_match)
       if expansion_score:
           return expansion_score

       if word.startswith(whole_match.lower()):
           return self._score_prefix(word, whole_match)

       return self._score_word_missing_letters(word, word_match)
       
    def _score_prefix(self, word, whole_match):
       """Helper method used by _score_word_match to find a score for
       an abbreviation which may be a prefix of the given word
       
       **INPUTS**
    
       *STR word* -- the word being matched

       *STR whole_match* -- the entire matching string for that word
       
       **OUTPUTS**

       *FLOAT conf_mod* -- confidence modifier for the word, with
       -1 < conf_mod <= 1.  If conf_mod < 0, then the confidence will
       be decreased and the match will be considered to be forbidden.
       If it is > 0, then the confidence will be increased.
       """
       k_prefix = 0.95
#       p_prefix = 1.0
       factor = 1 - k_prefix/len(whole_match)
#       factor = 1 - k_prefix/math.pow(len(whole_match), p_prefix)
#           factor = float(len(whole_match))/len(word)
       if tracing('SymDict._score_word_match'):
           trace('SymDict._score_word_match', 
               'prefix factor = %f' % factor)
       if word[-1] == 's' and whole_match[-1] != 's':
           factor = factor - 1.
       return factor

    def _score_word_missing_letters(self, word, word_match):
       """Helper method used by _score_word_match to find a score for
       a match which is not an abbreviation, an expansion, or a prefix
       of the word being matched.
       
       **INPUTS**
    
       *STR word* -- the word being matched

       *re.MatchObject word_match* -- the match for that word
       
       **OUTPUTS**

       *FLOAT conf_mod* -- confidence modifier for the word, with
       -1 < conf_mod <= 1.  If conf_mod < 0, then the confidence will
       be decreased.  If it is > 0, then the confidence will be
       increased.
       """
       leading_vowel = is_vowel(word[0])
       letter_matches = zip(word, word_match.groups(""))
       after_weak = 0
       penalty = 0.
       norm = 0.
       if tracing('SymDict._score_word_missing_letters'):
           trace('SymDict._score_word_missing_letters', 
               'letter matches = %s' % letter_matches)
       last_letter = ""
       for letter, letter_match in letter_matches:
           consonant = not is_vowel(letter)
           if consonant:
               norm = norm + 1.
           else:
               norm = norm + 0.5
           dropped = not letter_match
           if leading_vowel:
               if consonant:
                   leading_vowel = 0
               else:
                   consonant = 1
                   # dropping leading vowels has the same penalty as
                   # dropping consonant
           if after_weak:
               if letter in self.weak_pairs[after_weak]:
                   penalty = penalty - 0.5
                   # partially compensate for the penalty for the
                   # previous weak letter dropped
               after_weak = 0
           if dropped:
               if consonant:
                   if letter != last_letter:
                       penalty = penalty + 1.
                       if self.weak_pairs.has_key(letter):
                           after_weak = letter
               else:
                   penalty = penalty + 0.5
           last_letter = letter
       if tracing('SymDict._score_word_missing_letters'):
           trace('SymDict._score_word_missing_letters', 
               'penalty, norm = %f, %f' % (penalty, norm))
       penalty = penalty/norm
       # extra penalty for singular abbreviations of plural words
       if word[-1] == 's' and word_match.group()[-1] != 's':
           penalty = penalty + 1.0
       factor = 1. - penalty
       if tracing('SymDict._score_word_missing_letters'):
           trace('SymDict._score_word_missing_letters', 
               'factor = 1 - penalty/norm = %f' % factor)
       return factor
       
    def _score_abbrev(self, word, whole_match):
       """Helper method used by _score_word_match to find a score for
       a possible known abbreviation
       
       **INPUTS**
    
       *STR word* -- the word being matched

       *STR whole_match* -- the entire matching string for that word
       
       **OUTPUTS**

       *FLOAT conf_mod* -- confidence modifier, between 1 and 0.5, or
       None if there is no known abbreviation match.
       """
       abbreviations = []
       if self.abbreviations.has_key(word):
           abbreviations = self.abbreviations[word]
       if self.alt_abbreviations.has_key(word):
           abbreviations.extend(self.alt_abbreviations[word])
       if not abbreviations:
           return None
       try:
           i = abbreviations.index(whole_match)
           factor = 1. - 0.5 * float(i)/len(abbreviations)
           trace('SymDict._score_abbrev', 
               'factor = %f' % factor)
           return factor
       except ValueError:
           return None
       
    def _score_expansion(self, word, whole_match):
       """Helper method used by _score_word_match to find a score for
       a possible known expansion of a symbol       
       **INPUTS**
    
       *STR word* -- the word being matched

       *STR whole_match* -- the entire matching string for that word
       
       **OUTPUTS**

       *FLOAT conf_mod* -- confidence modifier, between 0.5 and 0, or
       None if there is no known abbreviation match.
       """
       l = len(whole_match)
       if l < 2:
           return None
       f = 1.
       if l == 2:
           f = 0.5
       if self.expansions.has_key(whole_match):
           if word in self.expansions[whole_match]:
               factor = f/(len(self.expansions[whole_match])  + 1.)
               trace('SymDict._score_expansion', 
                   'factor = %f' % factor)
               return factor
       return None
               
    def _remove_bad_symbol_matches(self, pseudo_symbol, words, native_matches):
       """Remove from *native_matches*, those symbols that match 
       *pseudo_symbol* but are too short, or otherwise deemed not
       significant.
       
       **INPUTS**

       *STR pseudo_symbol* -- The pseudo symbol that was matched.

       *[STR] words* -- words in the pseudo symbol

       *Iterator over Match objects native_matches* -- iterator of
       re.MatchObjects corresponding to native symbol matches for 
       *pseudo_symbol*.
       The first entry of every such list is the matched native symbol.
       
       **OUTPUTS**
       
       *[MatchObject] good_matches* -- list of native matches that are 
       deemed significant."""
       
       #
       # In case the word is spelled like "S. I. G."
       #
       pseudo_symbol = string.lower(pseudo_symbol)
       pseudo_symbol = re.sub('[^a-z0-9]', '', pseudo_symbol)
       trace('SymDict._remove_bad_symbol_matches', 
             'pseudo_symbol after is: "%s"' % pseudo_symbol)       
       
       good_matches = []
       for a_match in native_matches:
          native_sym = a_match.group(0)
          native_sym = native_sym.strip()
          trace('SymDict._remove_bad_symbol_matches', 
              'checking match with native symbol = "%s"' % native_sym)
          if native_sym == pseudo_symbol:
              good_matches.append(a_match)
              trace('SymDict._remove_bad_symbol_matches', 
                  'pseudo_symbol matches native_sym, so good match')
              continue
          trace('SymDict._remove_bad_symbol_matches', 
                  'len(native_sym) = %d' % len(native_sym))
          if self._native_symbol_too_short(native_sym):
              trace('SymDict._remove_bad_symbol_matches', 
                  'native_sym too short - skipping match')
              continue
          groups = a_match.groups()
          trace('SymDict._remove_bad_symbol_matches', 'groups=%s' % repr(groups))          
# this test looks at how many characters come from each word when
# abbreviations are run together.  It is designed to eliminate false
# matches like "do some more stuff" -> "do_some_stuff" (because "so"
# contains the first two characters of "some" and "me" contains the
# first and last characters of "more")
          run_together = 0          
          for i in range(1, len(groups)):
              trace('SymDict._remove_bad_symbol_matches', 
                    'checking length of group i=%s, len(groups[i])=%s, self.min_non_consec_chars_for_approx_match=%s, words[i-1]=%s' 
                    % (i, len(groups[i]), self.min_non_consec_chars_for_approx_match, words[i-1]))
          
              if len(groups[i]) < self.min_non_consec_chars_for_approx_match:
                  if not words[i-1].startswith(groups[i]):
                      trace('SymDict._remove_bad_symbol_matches', 
                          'too few non consec characters: %s vs. %s' \
                          % (words[i-1], groups[i]))
                      continue
                      
          for i in range(1, len(groups)-1):
              trace('SymDict._remove_bad_symbol_matches', 
                    'checking length of run-together group i=%s, groups[i]=%s, self.min_chars_run_together=%s' 
                    % (i, groups[i], self.min_chars_run_together))
          
              # Note: groups in a_match use 1-based indexes while entries in the
              #       groups list use 0-based indexes.          
              if self._group_matched_to_same_term_as_next(a_match, i+1):
                  # words run together
                  if not self._run_together_is_allowed(groups[i], words[i-1], groups[i+1], words[i]):
                     run_together = 1
                     break
                     
          if not run_together:
              trace('SymDict._remove_bad_symbol_matches', 
                  'adding to good_matches')
              good_matches.append(a_match)

       return good_matches
             
    def _native_symbol_too_short(self, native_sym):
       native_sym = re.sub('[^a-zA-Z0-9]', '', native_sym)
       return len(native_sym) < self.min_chars_for_approx_match
             
             
    def _group_matched_to_same_term_as_next(self, a_match, group_ind):
       """Returns true if groups *group_ind* and *group_ind+1* in a 
       symbol match were mapped to the same term of the native symbol.
       
       **INPUTS**

       *pythonregexpmatch a_match* -- A python regexp match object which was 
       created by matching regexp *self.reg_pseudo_to_native_symbol()* against a
       native symbol. 

       *INT group_ind* -- Index of the group to check.

       **OUTPUTS**
       
       *BOOL* -- True IIF the two groups were matched to the same term in the
       native symbol
       """
       end_cur = a_match.end(group_ind)
       start_next = a_match.start(group_ind+1)
       if end_cur == start_next:
          # No underscore or anything between the two terms.
          # Is there a change of case?
          last_char_cur = a_match.group(group_ind)[len(a_match.group(group_ind))-1]
          first_char_next = a_match.group(group_ind+1)[0]
          if not self._marks_term_change(last_char_cur, first_char_next):
             return 1
       return 0
       
    def _marks_term_change(self, char1, char2):
       """Returns true IIF going from *char1* to *char2* in a native symbol
       marks a transition between terms (for example, if we go from a lower
       case to an upper case character)"""
       
       type1 = self._char_type(char1)
       type2 = self._char_type(char2)
       
       if type1 == 'upper' and type2 != 'other':
          # Alphabetic characters that follow an uppercase character
          # are deemed to be part of a single capitalised or all-caps
          # word (this is not bullet proof, for example in APISpecs
          # I and S are not part of the same word)
          return 0
       else:
          return not type1 == type2
       
    def _char_type(self, character):
       """Returns a category for a character: 'lower', 'upper', 'other'"""
       if re.match('[a-z]', character):
          return 'lower'
       elif re.match('[A-Z]', character):
          return 'upper'
       else:
          return 'other'

    def _run_together_is_allowed(self, abbr1, word1, abbr2, word2):
       """Checks if abbreviations of two words can be run-together (i.e.
       concatenated without spaces, underscores or change of case) in
       a symbol match.
       
       Such running together is permitted IIF the two abbreviations are
       either long enough or are a prefix of the word they abbreviate.
       
       **INPUTS**

       *STR abbr1, abbr2* -- abbreviations for the first and second words
       
       *STR word1, word* -- first and second word
       
       **OUTPUTS**
       
       *BOOL* -- True IIF the symbol matching algorithm is allowed to run-together
       *abbr1* and *abbr2* into a single term of a native symbol.
       """
       abbr1 = string.lower(abbr1)
       abbr2 = string.lower(abbr2)       
       trace('SymDict._run_together_is_allowed', '** abbr1=%s, abbr2=%s, self.min_chars_run_together=%s' % (abbr1, abbr2, self.min_chars_run_together))
       if (len(abbr1) < self.min_chars_run_together and
           not word1.startswith(abbr1)):
          trace('SymDict._run_together_is_allowed', 
                'too few characters in words run together: %s' \
                       % abbr1)
           
          return 0
          
       if (len(abbr2) < self.min_chars_run_together and
           not word2.startswith(abbr2)):
          trace('SymDict._run_together_is_allowed', 
                'too few characters in words run together: %s' \
                       % abbr2)           
          return 0
          
       return 1

       
    def match_pseudo_symbol(self, pseudo_symbol, use_match_threshold=None):        
        """Returns a prioritized list of all known native symbols that
        match a given pseudo symbol.
        
        **INPUTS**
        
        *STR* pseudo_symbol -- The pseudo symbol to be matched. 
        
         *FLOAT use_match_threshold* -- minimum confidence level required of an
         approximate symbol match. If None, use the default setting of the
         SymDict.


        **OUTPUTS**
        
        *(good_matches, weak_matches, forbidden_matches)* --
         prioritized lists of good, weak (below threshold) and
         forbidden matches, with each match being a 2-tuple of confidence
         score between 0 and 1 (inclusive), and the written form of
         the native symbol
         
        
        .. [SymbolMatch] file:///./SymDict.SymbolMatch.html"""

        trace('SymDict.match_pseudo_symbol', 'pseudo_symbol=\'%s\'' % pseudo_symbol)
                
        if use_match_threshold == None:
           use_match_threshold = self.match_threshold                

        trace('SymDict.match_pseudo_symbol', '** Got use_match_threshold=%s' % use_match_threshold)

                
        if not pseudo_symbol:
            return ([], [], [])
        mess_symbols = lambda : "Symbols are: %s" % repr(self.symbol_info)
        if tracing('SymDict.match_pseudo_symbol'):
            trace('SymDict.match_pseudo_symbol', mess_symbols)


        #
        # Remove leading/trailing blanks
        #
        pseudo_symbol = re.sub('(^\s+|\s+$)', '', pseudo_symbol)
        words = re.split('[^a-zA-Z0-9]+', pseudo_symbol)


        #
        # Remove empty words in case pseudo_symbol starts or ends with
        # non-alphanums
        #
        if len(words) > 0 and words[0] == '':
            words = words[1:]
        if len(words) > 0 and words[len(words)-1] == '':
            words = words[:len(words)-1]
        #
        # Find all known native symbols that match *pseudo_symbol*
        #
        
        all_symbols = self.symbols_as_one_string(words[0][0])

            
#        print '-- SymDict.match_pseudo_symbols: words=%s' % words        
        regexp = self.reg_pseudo_to_native_symbol(words)

#        print '-- SymDict.match_pseudo_symbol: all_symbols=\'%s\'' % all_symbols
        
        raw_matches = regexp.finditer(all_symbols)

#        print '-- SymDict.match_pseudo_symbol: raw_matches = %s' % raw_matches

        allowed, forbidden = \
                      self._score_symbol_matches(pseudo_symbol, words, 
                                                 raw_matches)
        good_matches = []
        weak_matches = []
        if allowed:
            i_good = 0
            for score, match in allowed:
                if score >= use_match_threshold:
                    i_good = i_good + 1
                else:
                    break

            good_matches = allowed[0:i_good]
            weak_matches = allowed[i_good:]

        return good_matches, weak_matches, forbidden
        
    def reg_word_to_native(self, word, map_letter = None):
        """
        Returns a regular expression string for matching a single
        #word, with each of the non-initial letters being optional

        ** INPUTS **

        *STR word* -- the word to match

        *STR FCT(STR) map_letter* -- a function mapping the basic
        regular expression string for a single letter ("x" or "x?")
        to a new regular expression (e.g. "(x)" or "(x?)").  If
        omitted, defaults to the identity mapping.

        ** OUTPUTS **

        *STR* -- the final regular expression string
        """
        if map_letter is None:
            map_letter = lambda x: x
        s = map_letter(word[0])
        for letter in word[1: ]:
            s = s + map_letter(letter + '?')
        return s
            
    def reg_pseudo_to_native_symbol(self, words):
        
        """Returns a compiled regular expression that matches all possible
        native forms of a pseudo symbol.
        
        **INPUTS**
        
        *[STR]* words -- Words in the pseudo symbol to be matched.
        

        **OUTPUTS**
        
        *regexp* -- The regular expression. This regexp requires that
        the first character of every word in *words* be
        matched. Non alphanumeric characters are allowed between
        words. Matches for each word in *words* are put into
        groups.        
        """
        

#        print '-- SymDict.reg_pseudo_to_native_symbol: words=%s' % words
        if tracing('SymDict.reg_pseudo_to_native_symbol'): 
            trace('SymDict.reg_pseudo_to_native_symbol', 
                'words = %s' % repr(words))

        #
        # Generate string for the regexp.
        #
        # The regexp requires the first character of every word to be present.
        # Separator characters (not alphanums nor spaces) are allowed before
        # and after words.
        # 
        # For example for pseudo symbol "a new symbol", the regexp string
        # would be:
        #
        # ' [^a-zA-Z0-9\s]*((a)[^a-zA-Z0-9\s]*(ne{0,1}w{0,1})[^a-zA-Z0-9\s]*(sy{0,1}m{0,1}b{0,1}o{0,1}l{0,1}))[^a-zA-Z0-9\s]* '
        #
        # When matched, the first group corresponds to the whole symbol and
        # the remaining groups correspond to the segment of the native symbol
        # matched by each of the words in the pseudo symbol
        #

        reg_non_alphanums = '[^a-zA-Z0-9\s]*'
        regexp_string = ' (' + reg_non_alphanums
        for a_word in words:
            if tracing('SymDict.reg_pseudo_to_native_symbol'): 
                trace('SymDict.reg_pseudo_to_native_symbol', 
                    'a_word=%s' % a_word)
            if len(a_word) > 0:
                regexp_string = regexp_string + '(' + \
                    self.reg_word_to_native(a_word)
                regexp_string = regexp_string + ')' + reg_non_alphanums
        regexp_string = regexp_string +  ') '


        if tracing('SymDict.reg_pseudo_to_native_symbol'): 
            trace('SymDict.reg_pseudo_to_native_symbol', 
                'regexp_string="%s"' % regexp_string)
        
        #
        # Compile regexp with flags=IGNORECASE (i.e. case insensitive match)
        #
        regexp = re.compile(regexp_string, re.IGNORECASE)
        
        return regexp



    def accept_symbol_match(self, the_match):
        """Accepts a match between a pseudo symbol and its native form.

        Adds the new written\spoken symbol to the SR vocabulary and
        adds new abbreviations which are used in the match.

        Also, adds written\spoken symbols for symbols that contain
        those new abbreviations and whose spoken form can now be
        resolved because of those new abbreviations.
        
        **INPUTS**
        
        [SymbolMatch] the_match -- The match to be accepted
        

        **OUTPUTS**
        
        *none* --

        .. [SymbolMatch] file:///./SymDict.SymbolMatch.html"""

        if tracing('SymDict.accept_symbol_match'):
            trace('SymDict.accept_symbol_match', 
                'the_match.__dict__=%s' % (the_match.__dict__))
#        print '-- SymDict.accept_symbol_match: the_match.words=%s, the_match.word_matches=%s' % (the_match.words, the_match.word_matches)        
        
        #
        # Collapse consecutive single character abbreviations into a
        # single abbreviation
        # e.g. words=['context', 'sensitive']
        #      word_matches=['c', 's']
        #      then add abbreviation cs->'context sensitive' instead of
        #      two abbrevs c->'context' and 's'->'sensitive'
        #
        # Note: need to convert word_matches from a tuple to a list
        #       because can't use a tuple as an argument for +
        #
        the_match.word_matches = list(the_match.word_matches)
        the_match.word_matches, the_match.words = self.collapse_consec_single_chars(the_match.word_matches, the_match.words)                
                
        #
        # Convert word_matches back to tuple for consistency
        #
        the_match.word_matches = tuple(the_match.word_matches)
        
        #
        # Add newly resolved abbreviations
        #
        for ii in range(len(the_match.words)):
            abbrev = the_match.word_matches[ii]
            word = the_match.words[ii]
            if abbrev != word:
                if len(abbrev) >= min_abbreviation_len:
                    self.add_alt_abbreviations(word, [abbrev])
                else:
                    print 'WARNING: abbreviation \'%s\' not added (length < %s)' % (abbrev, min_abbreviation_len)            

        #
        # Add the word to the symbol dictionary, or add its spoken
        # forms if it is already there
        #
        
        self.add_symbol(the_match.native_symbol, 
        user_supplied_spoken_forms=[the_match.pseudo_symbol])


    def cleanup_dictionary(self, clean_sr_voc=0, clean_symdict=1, resave=1):
        """Cleans up the symbol dictionary.
        
        **INPUTS**
        
        *BOOL* clean_sr_voc=0 -- If true, then remove symbols from SR
        vocabulary

        *BOOL* clean_symdict=1 -- If true, then removes symbols from
         the symbol dictionary.

        *BOOL resave = 1* -- If true, symbol dictionary is
        resaved to disk after cleanup.        

        **OUTPUTS**
        
        *none* -- 
        """
        
#        print '-- SymDict.cleanup: called, self.standard_symbol_sources=%s' % self.standard_symbol_sources

        global vocabulary_symbols_with_written_form


        self._cached_symbols_as_one_string = {}

        #
        # Delete vocabulary entries for symbols
        #
        if clean_sr_voc:
            for (phrase, symbol_list) in \
                self.spoken_form_info.all_phrase_values():
                a_form = string.join(phrase)
        
                #
                # This spoken form was added specifically by VoiceCode.
                # Remove it.
                #
#                print '-- SymDict.cleanup: removing word %s' % a_form
                if not vocabulary_symbols_with_written_form:
                    #
                    # Just remove the spoken form
                    #
                    sr_interface.deleteWord(a_form)
                else:
                    #
                    # Remove every spoken\written entry in the vocabulary
                    #
                    for a_written_form in symbol_list:
                        entry = sr_interface.vocabulary_entry(a_form, a_written_form)
                        sr_interface.deleteWord(entry)

        #
        # Possibly clean up the symbol dictionary itself
        #
        if clean_symdict:
#            print '-- SymDict.cleanup: abbreviations are:'; self.print_abbreviations(show_unresolved=1)
            self.spoken_form_info = WordTrie.WordTrie()
            self.symbol_info = {}
            for an_unresolved in self.unresolved_abbreviations.keys():
#                print '-- SymDict.cleanup: removing unresolved abbreviation %s' % an_unresolved
                if self.expansions.has_key(an_unresolved):
                    del self.expansions[an_unresolved]
            self.unresolved_abbreviations = {}

        #
        # Recompile sources of standard symbols.
        # Add symbols to the SR vocabulary only if the SR vocabulary has been
        # cleansed of symbols. This is because addition of thouasands of
        # symbols to the SR vocabulary is slow.
        #
        if clean_sr_voc:
            add_sr_entries = 1
        else:
            add_sr_entries = 0
        if clean_sr_voc or clean_symdict:
            self.parse_symbols_from_files(self.standard_symbol_sources, add_sr_entries=add_sr_entries)

        #
        # Resave dictionary to disk
        #
        if resave: self.save()
            

    def _version_update(self, old_version):
        """private method to rename an old version of a persistent SymDict 
        file after we have successfully converted it to a new version

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        file = self.sym_file
        name, ext = os.path.splitext(file)
        backup = name + '.%d' % old_version + ext
        if os.path.exists(backup):
            shutil.copyfile(backup, backup + '.bak')
        shutil.copyfile(file, backup)
        os.remove(file)

    def _failed_read(self, msg):
        """private method to rename a persistent SymDict file if we fail 
        to read it.

        **INPUTS**

        *STR msg* -- reason for failure

        **OUTPUTS**

        *BOOL* -- true if we were able to recover abbreviations from an
        exported text file.
        """
        file = self.sym_file
        backup = file + '.failed'
        shutil.copyfile(file, backup)
        os.remove(file)

        fatal = 1
        recent = self.import_recent()
        if recent:
            fatal = 0

        msg = msg + 'We are renaming it to \n%s\n' % backup \
            + 'so that it will not recur next time you run the mediator\n'
        if fatal:
            msg = msg \
                + '\nHowever, if you proceed without correcting this\n' \
                + 'problem, you will lose all persistent known symbols\n' \
                + 'and abbreviation preferences.\n'
        else:
            msg = msg \
                + '\nWe were able to recover your recent abbreviation\n' \
                + 'preferences from \n%s\n' % recent \
                + 'However, if you have scanned any source files\n' \
                + 'for existing symbols, you will need to re-scan them.\n'

        msg = msg + '\nIf this error occurred following an update to a\n' \
                  + 'new version of VoiceCode, please submit a bug report.\n'
        if fatal:
            self.sym_file = None
        if self.interp:
            self.interp.input_error(msg, fatal)
        else:
            sys.stderr.write(msg)
            if fatal:
                raise RuntimeError(msg)
        return fatal

    def dict_from_file(self, on_failure = None):
        """Unpickles the dictionary containing a persistent version of
        the symbol dictionary.

        The file is *self.sym_file*. If *None*, don't reinitialise.
        
        **INPUTS**

        *FCT(STR) on_failure* -- callback function to call if we fail to
        read from the file.  Normally, this defaults to the _failed_read
        method, which renames the file and displays an error message
        so the user knows the consequences of the failure to read.  The
        only case when on_failure should be specified differently is
        during regression testing of the persistent SymDict system.
        
        **OUTPUTS**
        
        *{STR: ANY}* -- the dictionary, or None if we failed to read it.
        """
        if on_failure is None:
            on_failure = self._failed_read
        file = self.sym_file
        if file is None:
            return None
        if not os.path.exists(file):
            self.import_recent()
            return None
        try:
            f = open(file, "r")
        except:
            msg = 'WARNING: Failed to open symbol dictionary file\n%s\n' \
                % file
            on_failure(msg)
            return None
        try:
            try:
                values = cPickle.load(f)
            except:
                msg = 'WARNING: error reading symbol dictionary file\n%s\n' \
                    % self.sym_file
                raise ErrorReadingPersistDict(msg)
        except ErrorReadingPersistDict, e:
            f.close()
            on_failure(e.message)
            return None

        f.close()
        return values

    def init_from_file(self, on_failure = None):
        """Initialises the symbol dictionary from a persistent version
        stored on file.

        The file is *self.sym_file*. If *None*, don't reinitialise.
        
        **INPUTS**

        *FCT(STR) on_failure* -- callback function to call if we fail to
        read from the file.  Normally, this defaults to the _failed_read
        method, which renames the file and displays an error message
        so the user knows the consequences of the failure to read.  The
        only case when on_failure should be specified differently is
        during regression testing of the persistent SymDict system.
        
        **OUTPUTS**
        
        *BOOL* -- true if we successfully retrieved the state from the
        file
        """
        if on_failure is None:
            on_failure = self._failed_read
        values = self.dict_from_file()
        if values is None:
            return 0
        try:
            version = values['version']
        except KeyError:
            version = 1
        debug.trace('SymDict.init_from_file', 
            'state file version %s (current is %s)' \
            % (version, current_version))
        try:
            if version != current_version:
                if not symdict_cvtr.known_version(version):
                    debug.trace('SymDict.init_from_file', 
                        'version %d, known versions: %s' % (version, 
                        symdict_cvtr.known_versions()))
                    msg = 'unknown version %d' % version \
                        + ' of the symbol dictionary file\n%s\n' % self.sym_file
                    raise ErrorReadingPersistDict(msg)
                try:
                    values = symdict_cvtr.convert(values, version)
                except DictConverter.ConversionFailure, e:
                    msg = 'Error while trying to convert symbol dictionary file' \
                        + '\n%s\n' % self.sym_file \
                        + ' from version %s to %s\n' % (version, current_version)
                    msg = msg + str(e) + '\n'
                    raise ErrorReadingPersistDict(msg)
                else:
                    self._version_update(version)
            okay = self.init_from_dictionary(values)
            if okay:
                self.file_time = util.last_mod(self.sym_file)
                if version != current_version:
# don't export abbreviations automatically, because we want to use a different
# file name
                    self.save(export_abbreviations = 0)
            if okay:
                export_file = self.export_file + '.on_init.py'
                exported = self.export_abbreviations(export_file)
                if not exported:
                    msg = 'Warning: failure to export initial abbreviation\n'
                    msg = msg + 'preferences to abbrevs.on_init.py\n'
                    sys.stderr.write(msg)
            return okay
        except ErrorReadingPersistDict, e:
            on_failure(e.message)
            return 0


    def init_from_dictionary(self, dict, on_failure = None):
        """Initialises the symbol dictionary object from a dictionary of
        attributes.

        Note: init_from_dictionary can raise a number of different
        exceptions

        **INPUTS**
        
        *{STR: ANY} dict* -- dictionary of attributes necessary to
        initialize SymDict
        
        *FCT(STR) on_failure* -- callback function to call if we fail to
        read from the file.  Normally, this defaults to the _failed_read
        method, which renames the file and displays an error message
        so the user knows the consequences of the failure to read.  The
        only case when on_failure should be specified differently is
        during regression testing of the persistent SymDict system.
        
        **OUTPUTS**
        
        *BOOL* -- true if we successfully initialized from the
        dictionary
        """
        if on_failure is None:
            on_failure = self._failed_read
        try:
            try:
                version = dict['version']
            except KeyError:
                version = 1
            if version != current_version:
##{ wrong version
                msg = 'unknown version %d of the symbol dictionary file\n%s\n' % \
                    (version, self.sym_file)
                raise ErrorReadingPersistDict(msg)
##} wrong version
            fields = ['symbol_info', 'spoken_form_info', 'abbreviations', 
                'alt_abbreviations', 'unresolved_abbreviations', 
                'symbol_sources_read']
            current_field = None
            for field in fields:
                current_field = field
                if not dict.has_key(field):
##{ Missing field
                    msg = 'WARNING: missing field %s ' % field
                    ' reading %s from symbol dictionary file\n%s\n' \
                        % (current_field, self.sym_file)
                    raise ErrorReadingPersistDict(msg)
##} Missing field
        except ErrorReadingPersistDict:
## Cleanup (failed_read/rename here?)
            on_failure(msg)
            return 0

        try:
            self.symbol_info = dict['symbol_info']
            self.spoken_form_info = dict['spoken_form_info']
            self.abbreviations = dict['abbreviations'] 
            self.alt_abbreviations = dict['alt_abbreviations'] 
            self.acronyms = dict['acronyms']
            self.pronunciations = dict['pronunciations']
            self.extra_expansions = dict['extra_expansions']
            self.unresolved_abbreviations = dict['unresolved_abbreviations'] 
            self.symbol_sources_read = dict['symbol_sources_read']

# re-create expansions from abbreviations, etc.
            self.regenerate_expansions()

            #
            # Make sure symbols have been added to SR's vocabulary, and
            # to the dictionary of symbols starting with a given letter
            #
            for written_as, symbol_info in self.symbol_info.items():
                self._add_symbol_starting_with(written_as)
                for spoken_as in symbol_info.spoken_forms:
                    self.add_vocabulary_entry(written_as, spoken_as)

        except Exception, e:
            extype, value, trace = sys.exc_info()
            ex = traceback.format_exception(extype, value, trace)
            msg = 'Unexpected exception:\n'
            for line in ex:
                msg = msg + line
            msg = msg + '\nwhile initializing SymDict with dictionary data\n'
            msg = msg + ' read from symbol dictionary file\n%s\n' \
                  % self.sym_file
            on_failure(msg)
            return 0
        return 1

    def regenerate_expansions(self):
        """re-creates the dictionary of expansions from the dictionaries
        of abbreviations and alternate abbreviations

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.expansions = {}
        for word, abbreviations in self.abbreviations.items():
            for abbreviation in abbreviations:
                self._add_corresponding_expansion(abbreviation, word)
        for word, abbreviations in self.alt_abbreviations.items():
            for abbreviation in abbreviations:
                self._add_corresponding_expansion(abbreviation, word)
        for abbreviation, pronunciations in self.pronunciations.items():
            for pronunciation in pronunciations:
                self._add_corresponding_expansion(abbreviation, pronunciation)
        for abbreviation, extra_expansions in self.extra_expansions.items():
            for word in extra_expansions:
                self._add_corresponding_expansion(abbreviation, word)
        for acronym in self.acronyms.keys():
            acronym = string.lower(acronym)
            spoken = sr_interface.spoken_acronym(acronym)
            self._add_corresponding_expansion(abbreviation, spoken)

    def collapse_consec_single_chars(self, words, auxilliary_words=None):
        """Takes a list of words and collapse consecutive single-char words into single words
        
        **INPUTS**
        
        *[STR]* words -- List of words

        *[STR]* auxilliary_words = None -- List of words of the same
         length as *words*. Each word in list *words* corresponds to a
         word in list *auxilliary_words* and they are collapsed in
         sync. If None, set to be the same as *words*        

        **OUTPUTS**

        Returns tuple (collapsed_words, collapsed_auxilliary_words)
        
        *[STR]* collapsed_words -- List with consecutive single-char words collapsed.

        *[STR]* collapsed_auxilliary_words -- List with consecutive
         single-char words collapsed as per what was done to
         *collapsed_words*.
        
        """

        if auxilliary_words == None:
            auxilliary_words = copy.copy(words)
        
        #
        # Collapse consecutive words which are single characters into a single
        # word
        #
        collapsed_words = []
        collapsed_auxilliary_words = []
        re_single_char = '([a-zA-Z])\.{0,1}$'
        ii = 0
        while ii < len(words):
            new_word = words[ii]
            new_auxilliary_word = auxilliary_words[ii]
            a_match = re.match(re_single_char, new_word)
            if a_match:
                #
                # This word marks the start of a sequence of single character
                # words. new_word collects those characters.
                #
                new_word = a_match.group(1)
                ii = ii + 1                
                previous_was_char = 1
                while ii < len(words) and previous_was_char:
                    next_word = words[ii]
                    next_auxilliary_word = auxilliary_words[ii]
                    a_match = re.match(re_single_char, next_word)
                    if a_match:
                        ii = ii + 1
                        new_word = new_word + next_word
                        new_auxilliary_word = new_auxilliary_word + ' ' + next_auxilliary_word
                    else:
                        previous_was_char = 0
            else:
                ii = ii + 1
            collapsed_words = collapsed_words + [string.lower(new_word)]
            collapsed_auxilliary_words = collapsed_auxilliary_words + [new_auxilliary_word]
            
        return collapsed_words, collapsed_auxilliary_words

###############################################################################
# classes for updating old versions of the persistent SymDict dictionary
###############################################################################

class SymDictSingleConverter(DictConverter.SingleVersionDictConverter):
    def __init__(self, initial_version, final_version, **args):
        self.deep_construct(SymDictSingleConverter, 
            {'initial': initial_version, 
             'target': final_version
            }, 
            args)

    def initial_version(self):
        """the initial version from which DictConverter converts
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the initial version from which DictConverter converts
        """
        return self.initial

    def final_version(self):
        """the final version to which DictConverter converts
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the final version to which DictConverter converts
        """
        return self.target

    def dict_class(self):
        """indicates the class corresponding to the dictionaries
        we are converting

        **INPUTS**

        *none*

        **OUTPUTS**

        *CLASS* -- the class corresponding to the dictionaries
        """
        return SymDict

# obsolete because we switched file formats after version 2, but leave
# as an example for now
class AddSymbolSourcesRead(SymDictSingleConverter):

    def __init__(self, **args):
        self.deep_construct(AddSymbolSourcesRead, {}, args, 
           enforce_value = {'initial_version': 1, 
                            'final_version': 2})

    def convert(self, original, initial_version):
        """converts a dictionary from one version to another

        NOTE: If DictConverter is unable to convert the dictionary, it
        will raise a ConversionFailure exception

        **INPUTS**

        *INT initial_version* -- initial version of the dictionary

        *[ANY:ANY] original* -- original dictionary
        
        *none*

        **OUTPUTS**

        *[ANY:ANY]* -- final dictionary
        """
        if initial_version != self.initial:
            msg =  "unknown version %s" % initial_version
            raise DictConverter.ConversionFailure(msg)
        try:
            d = copy.deepcopy(original)
            d['symbol_sources_read'] = []
            d['version'] = self.final_version()
        except:
            extype, value, trace = sys.exc_info()
            ex = traceback.format_exception(extype, value, trace)
            msg = 'Unexpected exception:\n'
            for line in ex:
                msg = msg + line
            msg = msg + '\converting dictionary data\n'
            msg = msg + ' read from symbol dictionary file\n%s\n' \
                  % self.sym_file
            msg = msg + 'from version %s to %s' % (initial_version, 
                self.final_version())
            raise DictConverter.ConversionFailure(msg)
        return d

class SpokenFormsAsWordTrie(SymDictSingleConverter):

    def __init__(self, **args):
        self.deep_construct(AddSymbolSourcesRead, {}, args, 
           enforce_value = {'initial_version': 3, 
                            'final_version': 4})

    def convert(self, original, initial_version):
        """converts a dictionary from one version to another

        NOTE: If DictConverter is unable to convert the dictionary, it
        will raise a ConversionFailure exception

        **INPUTS**

        *INT initial_version* -- initial version of the dictionary

        *[ANY:ANY] original* -- original dictionary
        
        *none*

        **OUTPUTS**

        *[ANY:ANY]* -- final dictionary
        """
        if initial_version != self.initial:
            msg =  "unknown version %s" % initial_version
            raise DictConverter.ConversionFailure(msg)
        try:
            d = copy.deepcopy(original)
            d['version'] = self.final_version()
            new_spoken = WordTrie.WordTrie()
            for spoken_form, form_info in d['spoken_form_info'].items():
                phrase = string.split(spoken_form)
                new_spoken.add_phrase(phrase, form_info.symbols)
            d['spoken_form_info'] = new_spoken
        except:
            extype, value, trace = sys.exc_info()
            ex = traceback.format_exception(extype, value, trace)
            msg = 'Unexpected exception:\n'
            for line in ex:
                msg = msg + line
            msg = msg + '\converting dictionary data\n'
            msg = msg + ' read from symbol dictionary file\n%s\n' \
                  % self.sym_file
            msg = msg + 'from version %s to %s' % (initial_version, 
                self.final_version())
            raise DictConverter.ConversionFailure(msg)
        return d

class MoreExpansionCategories(SymDictSingleConverter):

    def __init__(self, **args):
        self.deep_construct(MoreExpansionCategories, {}, args, 
           enforce_value = {'initial_version': 4, 
                            'final_version': 5})

    def convert(self, original, initial_version):
        """converts a dictionary from one version to another

        NOTE: If DictConverter is unable to convert the dictionary, it
        will raise a ConversionFailure exception

        **INPUTS**

        *INT initial_version* -- initial version of the dictionary

        *[ANY:ANY] original* -- original dictionary
        
        *none*

        **OUTPUTS**

        *[ANY:ANY]* -- final dictionary
        """
        if initial_version != self.initial:
            msg =  "unknown version %s" % initial_version
            raise DictConverter.ConversionFailure(msg)
        try:
            d = copy.deepcopy(original)
            d['version'] = self.final_version()
            d['acronyms'] = {}
            d['pronunciations'] = {}
            d['extra_expansions'] = {}
        except:
            extype, value, trace = sys.exc_info()
            ex = traceback.format_exception(extype, value, trace)
            msg = 'Unexpected exception:\n'
            for line in ex:
                msg = msg + line
            msg = msg + '\converting dictionary data\n'
            msg = msg + ' read from symbol dictionary file\n%s\n' \
                  % self.sym_file
            msg = msg + 'from version %s to %s' % (initial_version, 
                self.final_version())
            raise DictConverter.ConversionFailure(msg)
        return d


# global converter
symdict_cvtr = DictConverter.CompoundDictConverter(SymDict, current_version)

# when incrementing current_version and adding a new converter (from the
# previous version to the current one) to the compound converter, 
# you must it prepend the new converter to this list:

# obsolete because we switched file formats after version 2
#symdict_cvtr.add_converter(AddSymbolSourcesRead())

symdict_cvtr.add_converter(MoreExpansionCategories())
symdict_cvtr.add_converter(SpokenFormsAsWordTrie())

###############################################################################
# Configuration functions. These are not methods
###############################################################################

def define_language(name, definition):
    """Defines the syntax of a programming language.

    **INPUTS**

    *STR* name -- name of the programming language

    [LangDef] definition -- language definition 


    **OUTPUTS**

    *none* -- 

    .. [LangDef] file:///./LangDef.LangDef.html"""

    global language_definitions
    definition.name = name
    language_definitions[name] = definition

################################################################
# Miscellaneous functions
################################################################

def is_vowel(letter):
    return letter in "aeiou"


    

# defaults for vim - otherwise ignore
# vim:sw=4
