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

"""helper functions which generate LSAs and CSCs"""

import string
import re
from SpacingState import *
from CSCmd import CSCmd
from CmdInterp import LSAlias, CSCmdSet, LSAliasSet
from cont_gen import *
from actions_gen import *
from Object import Object
import sr_interface
import debug
from SpacingState import *
from vc_globals import *

# US English military spelling

alpha_bravo = {}
alpha_bravo["a"] = "alpha"
alpha_bravo["b"] = "bravo"
alpha_bravo["c"] = "charlie"
alpha_bravo["d"] = "delta"
alpha_bravo["e"] = "echo"
alpha_bravo["f"] = "foxtrot"
alpha_bravo["g"] = "golf"
alpha_bravo["h"] = "hotel"
alpha_bravo["i"] = "india"
alpha_bravo["j"] = "juliett"
alpha_bravo["k"] = "kilo"
alpha_bravo["l"] = "lima"
alpha_bravo["m"] = "mike"
alpha_bravo["n"] = "november"
alpha_bravo["o"] = "oscar"
alpha_bravo["p"] = "papa"
alpha_bravo["q"] = "quebec"
alpha_bravo["r"] = "romeo"
alpha_bravo["s"] = "sierra"
alpha_bravo["t"] = "tango"
alpha_bravo["u"] = "uniform"
alpha_bravo["v"] = "victor"
alpha_bravo["w"] = "whiskey"
alpha_bravo["x"] = "xray"
alpha_bravo["y"] = "yankee"
alpha_bravo["z"] = "zulu"

def add_letters(aliases, prefix, alphabet = 'abcdefghijklmnopqrstuvwxyz', 
    name_map = alpha_bravo, language = None):
    """define LSAs for alpha, etc. or letter-alpha, etc.

    **INPUTS**

    *LSAliasSet* -- set to which to add the aliases 

    *STR prefix* -- prefix to add to the letter names (e.g. "" for bare
    letters, or "letter-" to mimic Natspeak behavior)

    *STR* alphabet -- set of letters for which to define aliases

    *{STR: STR}* -- map from letters to their spoken form (e.g.
    the military alphabet)

    *STR language* -- language of the aliases, or None to use in all
    languages (almost certainly the desired behavior)
    """
    if language == None:
        language = all_languages
    for letter in alphabet:
        try:
            name = "%s" % name_map[letter]
            spoken = "%s%s" % (prefix, name)
            an_LSA = LSAlias(spoken_forms = [spoken], 
                meanings = {language: letter}, new_symbol = 'start')
            aliases.add_lsa(an_LSA)
        except KeyError:
            pass

    
    
def add_escaped_characters(commands, back_slash = 'back slash', 
    alphabet = 'abcdefghijklmnopqrstuvwxyz',
    name_map = alpha_bravo, cap = "cap", context = None):
    """define CSCs for characters escaped with backslashes

    **INPUTS**

    *CSCmdSet commands* -- set to which to add the context-sensitive
    commands

    *STR* back_slash -- spoken form for the backslash

    *STR* alphabet -- set of letters for which to define escaped
    characters

    *{STR: STR}* -- map from letters to an alternate spoken form (e.g.
    the military alphabet), or None to omit

    *STR* cap -- spoken form indicating that the letter should be
    capitalized, or None to omit capitalized forms

    *Context* context -- the context in which the CSCs should apply, or
    None for ContAny()
    """
    if context is None:
        context = ContAny()
    for letter in alphabet:
        ending = ["%s." % string.upper(letter)]
        if name_map:
            try:
                named = "%s" % name_map[letter]
            except KeyError:
                pass
            else:
                ending.append(named)
        spoken = map(lambda s, prefix = back_slash: "%s %s" % (prefix, s),
                     ending)
        acmd = CSCmd(spoken_forms = spoken, 
            meanings = {context: ActionInsert('\\%s' % letter, '',
                spacing = no_space_before | no_space_after)},
                docstring = 'escaped character')
        commands.add_csc(acmd)
        if cap:
            cap_spoken = map(lambda s, prefix = ("%s %s" % (back_slash, cap)): \
               "%s %s" % (prefix, s), ending)
            acmd = CSCmd(spoken_forms = cap_spoken, meanings = \
                {context: ActionInsert('\\%s' % string.upper(letter), '',
                    spacing = no_space_before | no_space_after)},
                    docstring = 'cap escaped character')
            commands.add_csc(acmd)


def add_backspacing(commands, max_count = 5, primary = 'back space', 
    alternate = 'delete backwards', context = None):
    """add CSCs for repeatable backspacing commands.

    Note: NaturallySpeaking (at least version 6) has a backspace command
    and a backspace 2 to 20 command.  Since they are defined as command
    grammars, they will take precedence over our "back space" dictation
    commands (except if you say the latter immediately before/after
    something else)

    **INPUTS**

    *CSCmdSet commands* -- set to which to add the context-sensitive
    commands

    *INT* max_count -- generate commands for backspace and backspace 2
    through max_count

    *STR* primary -- primary spoken form

    *STR* alternate -- alternate spoken form, or None to omit

    *Context* context -- the context in which the CSCs should apply, or
    None for ContAny()
    """
    if context is None:
        context = ContAny()
    for i in range(1, max_count + 1):
        count_string = "%s" % i
        spoken = ["%s %s" % (primary, count_string)]
        if alternate:
            spoken.append("%s %s" % (alternate, count_string))
        if i == 1:
            spoken = [primary]
            if alternate:
                spoken.append(alternate)
        command = CSCmd(spoken_forms = spoken, 
            meanings = {context: ActionBackspace(n_times = i)},
            docstring = "Backspace %d characters" % i,
            generate_discrete_cmd = 1)
        commands.add_csc(command)

def add_repeats(commands, max_count = 10, context = None, again = None, 
    time = 'time', times = 'times'):
    """add CSCs for repeating commands.

    **INPUTS**

    *CSCmdSet commands* -- set to which to add the context-sensitive
    commands

    *INT* max_count -- generate commands for repeating 1 through
    max_count times

    *Context* context -- the context in which the CSCs should apply, or
    None for ContLastActionWas([ActionRepeatable])

    *[STR]* again -- list of prefix strings for again/repeat n times, or
    None for ['again', 'repeat']

    *STR* time -- singular string for 1 time

    *STR* times -- plural string for n times
    """
    if context is None:
        context = ContLastActionWas([ActionRepeatable])
    if again is None:
        again = ['again', 'repeat']
    
    spoken_numerals = ['one', 'two', 'three', 'four', 'five', 
                       'six', 'seven', 'eight', 'nine', 'ten']

    for i in range(1, max_count + 1):
        count_string = "%s" % i
        if i == 1:
            initial_count = ["%d %s" % (1, time), "one %s" % time]
        else:
            initial_count = ["%d %s" % (i, times)]
            if len(spoken_numerals) >= i:
                initial_count.append("%s %s" % (spoken_numerals[i-1], times))
            
        
        commands.add_csc(CSCmd(spoken_forms = initial_count, 
            meanings = {context: ActionRepeatLastCmd(n_times = i,
            check_already_repeated = 1)},
            docstring = "Perform last command %s" % initial_count[0]))
        more = []
        for prefix in again:
            for form in initial_count:
                more.append("%s %s" % (prefix, form))
        commands.add_csc(CSCmd(spoken_forms = more, 
            meanings = {context: ActionRepeatLastCmd(n_times = i)},
            docstring = "Repeat last command %s" % initial_count[0]))


class PunctuationSet(Object):
    """abstract class for sets of punctuation for which to generate
    language-specific aliases for dictation and context-sensitive
    commands for navigation

    **INSTANCE ATTRIBUTES**

    *STR name* -- name of the LSAliasSet to use when adding LSAs.  The
    suffix " navigation" will be added to generat the name of the CSCmdSet
    when adding punctuation navigation 
    
    *STR language* -- language of the punctuation LSAs

    *Context context* -- context for punctuation navigation commands

    *STR next_word, prev_word, after_word, before_word* -- words with 
    which to prefix navigate-by-punctuation commands generated from 
    the punctuation set
    """
    def __init__(self, name, language = None, context = None, **args):
        """
        NOTE: These parameters can also be set later

        **INPUTS**

        *STR name* -- name of the LSAliasSet to use when adding LSAs.  The
        suffix " navigation" will be added to generate the name of the CSCmdSet
        when adding punctuation navigation 

        *STR language* -- language of the punctuation LSAs, or None to
        add language-independent aliases

        *Context context* -- context for punctuation navigation
        commands, or None for ContAny.
        """
        if language == None:
            language = all_languages
        self.deep_construct(PunctuationSet,
                            {'name': name,
                             'language': language,
                             'context': context,
                             'next_word': None,
                             'prev_word': None,
                             'after_word': None,
                             'before_word': None},
                            args)
        self.set_direction_words()
        self.set_side_words()

    def set_direction_words(self, prev_word = 'previous', next_word = 'next'):
        """set the values of direction words used in
        navigation-by-punctuation

        **INPUTS**

        *STR prev_word, next_word* -- prefixes for moving to 
        previous/next punctuation mark

        **OUTPUTS**

        *none*
        """
        self.next_word = next_word
        self.prev_word = prev_word

    def set_side_words(self, before_word = 'before', after_word = 'after'):
        """set the values of words indicating where the cursor should be
        placed in navigation-by-punctuation

        **INPUTS**

        *STR before_word, after_word* -- prefixes for 
        moving before/after the punctuation mark

        **OUTPUTS**

        *none*
        """
        self.before_word = before_word
        self.after_word = after_word

    def set_name(self, name):
        """change the name to be used when adding LSAs and CSCs as a set

        **INPUTS**

        *STR name* -- name of the LSAliasSet to use when adding LSAs.  The
        suffix " navigation" will be added to generat the name of the CSCmdSet
        when adding punctuation navigation 

        **OUTPUTS**

        *none*
        """
        self.name = name

    def set_language(self, language):
        """set the target language

        **INPUTS**

        *STR language* -- language of the punctuation LSAs, or None to
        add language-independent aliases

        **OUTPUTS**

        *none*
        """
        self.language = language

    def set_context(self, context):
        """set the target language

        **INPUTS**

        *Context context* -- context for punctuation navigation
        commands, or None for ContAny.

        **OUTPUTS**

        *none*
        """
        self.context = context

    def _add_lsa(self, aliases, written, spoken_forms, spacing,
        new_symbol = None):
        """private method to add an LSA for dictation of this 
        punctuation symbol
        
        **INPUTS**

        *LSAliasSet aliases* -- set to which to add the language-specific
        aliases 
        
        *STR written* -- the written form of the symbol

        *[STR] spoken_forms* -- the spoken forms

        *INT spacing* -- the spacing flags (see SpacingState.py)

        *STR* new_symbol -- flag indicating whether the punctuation
        symbol part of a new identifier.  Recognized values are None if 
        it cannot, 'start' if it can start a new symbol (e.g.
        underscore), or 'within' if it can appear within a
        symbol but cannot start one.
        """
        aliases.add_lsa(LSAlias(spoken_forms, 
                        {self.language: written}, spacing, 
                        new_symbol = new_symbol))

    def _add_navigation(self, commands, expression, spoken_forms):
        """private method to add CSCs for navigation by this 
        punctuation symbol
        
        **INPUTS**

        *CSCmdSet commands* -- set to which to add the context-sensitive
        commands

        *STR expression* -- a regular expression matching the symbol

        *[STR] spoken_forms* -- the spoken forms
        """
        context = self.context
        if context is None:
            context = ContAny()
        debug.trace('PunctuationSet._add_navigation', 'self.name=%s, context=%s, expression="%s", spoken_forms=%s' % (self.name, context, expression, spoken_forms))
        for spoken in spoken_forms:
            command = CSCmd(spoken_forms = \
               ['%s %s' % (self.next_word, spoken),
                '%s %s' % (self.after_word, spoken),
                '%s %s %s' % (self.after_word, self.next_word, spoken)],
               meanings = {context: ActionSearchOrLookback(regexp =
                   expression, extra_space = 1)},
               docstring='go after next %s' % spoken)
            debug.trace('PunctuationSet._add_navigation', 'command.meanings=%s, command.spoken_forms=%s' % (command.meanings, repr(command.spoken_forms)))
            commands.add_csc(command)

            command = CSCmd(spoken_forms = \
               ['%s %s %s' % (self.before_word, self.next_word, spoken), 
                '%s %s' % (self.before_word, spoken)],
               meanings = {context: ActionSearchOrLookback(regexp =
                   expression, direction = 1, where = -1, extra_space = 1)},
               docstring='go before next %s' % spoken)
            commands.add_csc(command)

            command = CSCmd(spoken_forms = \
               ['%s %s' % (self.prev_word, spoken), 
                '%s %s %s' % (self.after_word, self.prev_word, spoken)],
               meanings = {context: ActionSearchOrLookback(regexp =
                   expression, direction = -1, extra_space = 1)},
               docstring='go after previous %s' % spoken)
            commands.add_csc(command)
            command = CSCmd(spoken_forms = \
               ['%s %s %s' % (self.before_word, self.prev_word, spoken)],
               meanings = {context: ActionSearchOrLookback(regexp =
                   expression, direction = -1, where = -1, extra_space = 1)},
               docstring='go before previous %s' % spoken)
            commands.add_csc(command)
        debug.trace('PunctuationSet._add_navigation', 'exited')


class SinglePunctuation(PunctuationSet):
    """set of individual (i.e. not paired) punctuation symbols used to
    define punctuation navigation commands and LSAs

    **INSTANCE ATTRIBUTES**

    *[STR]* written_forms -- list of written forms

    *[[STR]]* spoken_forms -- list of corresponding (lists of) spoken forms 

    *[INT]* spacing -- corresponding spacing flags

    *[STR]* new_symbol -- corresponding flags indicating whether the 
    punctuation symbol part of a new identifier.  Recognized values 
    are None if it cannot, 'start' if it can start a new symbol (e.g.
    underscore), or 'within' if it can appear within a
    symbol but cannot start one.
    """
    def __init__(self, **args):
        self.deep_construct(SinglePunctuation,
                            {'written_forms': [],
                             'spoken_forms': [],
                             'spacing': [],
                             'new_symbol': []
                            },
                            args)

    def add(self, written_form, spoken_forms, spacing = 0, 
        new_symbol = None):
        """add a punctuation symbol

        **INPUTS**

        *STR* written_form -- written form

        *[STR]* spoken_forms -- list of corresponding spoken forms 

        *INT spacing* -- the spacing flags (see SpacingState.py)

        *STR* new_symbol -- flag indicating whether the punctuation
        symbol part of a new identifier.  Recognized values are None if 
        it cannot, 'start' if it can start a new symbol (e.g.
        underscore), or 'within' if it can appear within a
        symbol but cannot start one.

        **OUTPUTS**

        *none*
        """
        self.written_forms.append(written_form)
        self.spoken_forms.append(spoken_forms)
        self.spacing.append(spacing)
        self.new_symbol.append(new_symbol)
    
    def create(self, interp, force = 0, dictation_only = 0):
        """add LSAs for dictation of punctuation symbols and CSCs for
        punctuation navigation
        
        **INPUTS**
        
        *CmdInterp interp* -- command interpreter (or NewMediatorObject,
        which will forward to the interpreter) to which to add the LSAs
        and CSCs

        *BOOL force* -- if true, create aliases even when another alias
        with the same spoken form exists (normally, for standard forms,
        we do not do this).

        *BOOL dictation_only* -- if true, add only LSAs for dictation 
        of punctuation symbols, but not punctuation navigation commands
        """
        debug.trace('SinglePunctuation.create', 'self.name="%s"' % self.name)
        if not interp:
            return
        aliases = LSAliasSet(self.name, description = 'dictating punctuation')
        commands = CSCmdSet(self.name + " navigation", 
            description = 'navigation by punctuation')
        for i in range(len(self.written_forms)):
            if force:
                add_spoken_forms = self.spoken_forms[i]
            else:
                add_spoken_forms = []
                for spoken in self.spoken_forms[i]:
# only add the word if we don't already have an LSA with the same spoken
# form, and if the word exists in the vocabulary.  The latter prevents us 
# from accidentally adding back punctuation which the user has deleted from
# the vocabulary, or adding NaturallySpeaking US English spoken forms 
# for punctuation to the vocabulary of a different edition or different
# speech engine - DFC
#
# Actually, checking if word exists in the vocab, has been causing a lot of problems 
# due to the fact that different versions of NatSpeak
# use different voc entries for punctuation. The latest in date is that 
# backslash is not being added in NatSpeak 8 and 9, because the word does not 
# exist, and therefore when the user says "backslash", nothing happens.
# So... just add the punctuation mark, even if there is no voc entry for it 
# in the vocab. Worst case scenario is that you will add a spoken form which 
# is reasonable, but does not correspond to the spoken form used in that version 
# of NatSpeak. This is much better than having the punctuation mark not work - AD

                    if interp.has_lsa(spoken, language = self.language):
                        debug.trace('SinglePunctuation.create',
                            'single form "%s" already exists in language %s' % (spoken, self.language))
                        continue
                    entry = sr_interface.vocabulary_entry(spoken,
                        self.written_forms[i])
                    add_spoken_forms.append(spoken)
            if add_spoken_forms:
                self._add_lsa(aliases, self.written_forms[i], 
                    add_spoken_forms, self.spacing[i], new_symbol =
                    self.new_symbol[i])
                if not dictation_only:
                    self._add_single_navigation(commands, i, add_spoken_forms)
        interp.add_lsa_set(aliases)
        interp.add_csc_set(commands)

    def _add_single_navigation(self, commands, i, spoken_forms):
        """private method to add CSCs for navigation by this 
        punctuation symbol
        
        **INPUTS**

        *CSCmdSet commands* -- set to which to add the context-sensitive
        commands

        *INT i* -- index into list of written forms

        *[STR] spoken_forms* -- the spoken forms
        """
        debug.trace('SinglePunctuation._add_single_navigation', 'spoken_forms=%s' % repr(spoken_forms))
        escaped = re.escape(self.written_forms[i])
        self._add_navigation(commands, expression = escaped,
            spoken_forms = spoken_forms) 

class PairedPunctuation(PunctuationSet):
    """partially concrete class used to define punctuation navigation 
    commands and LSAs for punctuation symbols which come in matching pairs. 

    **INSTANCE ATTRIBUTES**

    *[STR] empty_prefixes* -- list of prefixes to singular form(s) for 
    dictating an empty pair of symbols

    *[STR] singular_pair* -- list of format strings (containing %s for
    which the singular form of the symbol name will be substituted)
    to create spoken forms for dictating a pair of symbols 
    and placing the cursor in the middle.

    *[STR] plural_pair* -- list of format strings (containing %s for
    which the plural form of the symbol name will be substituted)
    to create spoken forms for dictating a pair of symbols 
    and placing the cursor in the middle.

    *STR out_of* -- prefix for commands to jump forward out of a pair of
    symbols

    *STR back* -- prefix to out_of for commands to jump back out of a pair of
    symbols
    """
    def __init__(self, singular_pair = None, plural_pair = None, **args):
        """
        **INPUTS**
        """
        self.deep_construct(PairedPunctuation,
                            {'singular_pair': singular_pair,
                             'plural_pair': plural_pair,
                             'out_of': None,
                             'back': None,
                             'empty_prefixes': None},
                            args)
        self.set_empty_prefixes()
        self.set_jump_out_words()

    def set_pair_forms(self, singular_pair = None, plural_pair = None):
        """set the format strings for spoken forms for dictating pairs 
        of symbols.

        **INPUTS**

        *[STR] singular_pair* -- list of format strings (containing %s for
        which the singular form of the symbol name will be substituted)
        to create spoken forms for dictating a pair of symbols 
        and placing the cursor in the middle.

        *[STR] plural_pair* -- list of format strings (containing %s for
        which the plural form of the symbol name will be substituted)
        to create spoken forms for dictating a pair of symbols 

        **OUTPUTS**

        *none*
        """
        self.singular_pair = singular_pair
        self.plural_pair = plural_pair

    def set_jump_out_words(self, out_of = 'out of', back = 'back'):
        """set the values of words used in commands to jump out of a
        pair of symbols

        **INPUTS**

        *STR out_of* -- prefix for commands to jump forward out of a 
        pair of symbols

        *STR back* -- prefix to out_of for commands to jump back out 
        of a pair of symbols
        **OUTPUTS**

        *none*
        """
        self.out_of = out_of
        self.back = back

    def set_empty_prefixes(self, empty_prefixes = None):
        """set the prefixes for the left/open and right/close spoken forms

        **INPUTS**

        *[STR] empty_prefixes* -- list of prefixes to singular form(s) for 
        dictating an empty pair of symbols

        **OUTPUTS**

        *none*
        """
        if empty_prefixes is None:
            empty_prefixes = ['empty']
        self.empty_prefixes = empty_prefixes

    def _add_empty(self, aliases, written, plural, spacing):
        """private method to add LSAs for dictation of empty pairs 
        of symbols
        
        **INPUTS**

        *LSAliasSet aliases* -- set to which to add the language-specific
        aliases 

        *STR written* -- the written form of the empty pair
        
        *[STR] plural* -- list of plural spoken forms for the symbol

        *INT spacing* -- the spacing flags (see SpacingState.py)
        """
        if self.empty_prefixes:
            empty_forms = []
            for spoken in plural:
                for empty in self.empty_prefixes:
                    empty_forms.append("%s %s" % (empty, spoken))
            if empty_forms:
                aliases.add_lsa(LSAlias(empty_forms, {self.language: written},
                        spacing))

    def _add_between(self, commands, action, singular, plural):
        """private method to add CSCs for dictating a pair of symbols 
        and placing the cursor in the middle.
        
        **INPUTS**

        *CSCmdSet commands* -- set to which to add the context-sensitive
        commands

        *ActionInsert action* -- action which adds the pair of
        symbols around the cursor

        *[STR] singular* -- list of singular spoken forms for the symbol
        
        *[STR] plural* -- list of plural spoken forms for the symbol
        """
        context = self.context
        if context is None:
            context = ContAny()
        forms = []
        if self.singular_pair:
            for spoken in singular:
                for form in self.singular_pair:
                    forms.append(form % spoken)
        if self.plural_pair:
            for spoken in plural:
                for form in self.plural_pair:
                    forms.append(form % spoken)
        doc = 'pair of %s' % plural[0]
        commands.add_csc(CSCmd(spoken_forms = forms, 
                               meanings = {context: action},
                               docstring = doc))

    def _add_jump_out(self, commands, open_written, close_written, plural):
        """private method to add CSCs for jumping out of a pair of
        symbols.
        
        **INPUTS**

        *CSCmdSet commands* -- set to which to add the context-sensitive
        commands

        *STR open_written* -- written form for the opening symbol of the pair

        *STR close_written* -- written form for the closing symbol of the pair

        *[STR] plural* -- list of plural spoken forms for the symbol
        """
        context = self.context
        if context is None:
            context = ContAny()
        open_escaped = re.escape(open_written)
        close_escaped = re.escape(close_written)
        if self.out_of:
            doc = 'jump forward out of innermost pair of %s' \
                % plural[0]
            spoken_forms = []
            for spoken in plural:
                spoken_forms.append("%s %s" % (self.out_of, spoken))
                
            command = CSCmd(spoken_forms,
                meanings = {context: ActionSearchOrLookback(regexp =
                close_escaped, extra_space = 1)},
                docstring=doc)
            commands.add_csc(command)
            if self.back:
                back_spoken_forms = []
                doc = 'jump backward out of innermost pair of %s' \
                    % plural[0]
                for spoken_form in spoken_forms:
                    back_spoken_forms.append(self.back + " " + spoken_form)
                command = CSCmd(back_spoken_forms,
                meanings = {context: ActionSearchOrLookback(regexp =
                   open_escaped, direction = -1, where = -1, extra_space = 1)},
                   docstring=doc)
                commands.add_csc(command)


class LeftRightPunctuation(PairedPunctuation):
    """punctuation symbols which come in matching pairs with different
    written forms for left/open vs. right/close 
    used to define punctuation navigation commands and LSAs

    **INSTANCE ATTRIBUTES**

    *[STR]* open_written_forms, close_written_forms -- opening and closing
    written forms for the pair

    *[[STR]]* singular_spoken_forms -- corresponding (lists of) spoken forms 
    for single symbols (omitting open/left/close/right)

    *[[STR]]* plural_spoken_forms -- corresponding (lists of) spoken forms 
    for plural symbols

    *[STR] left_prefixes* -- prefixes for left/open member of the pair

    *[STR] right_prefixes* -- prefixes for right/close member of the pair
    """
    def __init__(self, **args):
        self.deep_construct(LeftRightPunctuation,
                            {'open_written_forms': [],
                             'close_written_forms': [],
                             'singular_spoken_forms': [],
                             'plural_spoken_forms': [],
                             'left_prefixes': None,
                             'right_prefixes': None},
                            args)
        self.set_prefixes()

    def set_prefixes(self, left_prefixes = None, right_prefixes = None):
        """set the prefixes for the left/open and right/close spoken forms

        **INPUTS**

        *[STR] left_prefixes* -- prefixes for left/open member of the pair, 
        or None for ['left-', 'open-']

        *[STR] right_prefixes* -- prefixes for right/close member of the pair,
        or None for ['right-', 'close-']

        **OUTPUTS**

        *none*
        """
        if left_prefixes is None:
            left_prefixes = ['left-', 'open-']
        if right_prefixes is None:
            right_prefixes = ['right-', 'close-']
        self.left_prefixes = left_prefixes
        self.right_prefixes = right_prefixes

    def add(self, open_written_form, close_written_form, 
        singular_spoken_forms, plural_spoken_forms = None):
        """
        add a pair of punctuation symbols

        **INPUTS**

        *STR* open_written_form, close_written_form -- opening and closing
        written forms for the pair

        *[STR]* singular_spoken_forms -- spoken forms for single symbols
        (omitting open/left/close/right)

        *[STR]* plural_spoken_forms -- spoken forms for plural symbols

        **OUTPUTS**

        *none*
        """
        self.open_written_forms.append(open_written_form)
        self.close_written_forms.append(close_written_form)
        self.singular_spoken_forms.append(singular_spoken_forms)
        self.plural_spoken_forms.append(plural_spoken_forms)

    def create(self, interp, force = 0, dictation_only = 0):
        """add LSAs for dictation of punctuation symbols and CSCs for
        punctuation navigation
        
        **INPUTS**
        
        *CmdInterp interp* -- command interpreter (or NewMediatorObject,
        which will forward to the interpreter) to which to add the LSAs
        and CSCs

        *BOOL force* -- if true, create aliases even when another alias
        with the same spoken form exists (normally, for standard forms,
        we do not do this).

        *BOOL dictation_only* -- if true, add only LSAs for dictation 
        of punctuation symbols, but not punctuation navigation commands
        """
        if not interp:
            return
        aliases = LSAliasSet(self.name, 
            description = 'dictating left- and right- punctuation')
        between = CSCmdSet("between " + self.name,
            description = 'dictating punctuation in matching pairs')
        navigation = CSCmdSet(self.name + " navigation", 
            description = 'navigation by punctuation')
        for i in range(len(self.open_written_forms)):
            open_spoken_forms = []
            close_spoken_forms = []
            for spoken in self.singular_spoken_forms[i]:
                for left in self.left_prefixes:
                    left_comp = "%s%s" % (left, spoken)
                    if force:
                        open_spoken_forms.append(left_comp)
                    else:
# only add the word if we don't already have an LSA with the same spoken
# form, and if the word exists in the vocabulary.  The latter prevents us 
# from accidentally adding back punctuation which the user has deleted from
# the vocabulary, or adding NaturallySpeaking US English spoken forms 
# for punctuation to the vocabulary of a different edition or different
# speech engine
                        if interp.has_lsa(left_comp, language = self.language):
                            debug.trace('LeftRightPunctuation.create',
                                'left form "%s" already exists in language %s' % (left_comp, self.language))
                            continue
                        entry = sr_interface.vocabulary_entry(left_comp,
                            self.open_written_forms[i])
                        if sr_interface.word_exists(entry):
                            open_spoken_forms.append(left_comp)
                        else:
                            debug.trace('LeftWritePunctuation.create',
                                "word '%s' doesn't exist" % entry)
                for right in self.right_prefixes:
                    right_comp = "%s%s" % (right, spoken)
                    if force:
                        close_spoken_forms.append(right_comp)
                    else:
                        if interp.has_lsa(right_comp, 
                            language = self.language):
                            debug.trace('LeftRightPunctuation.create',
                                'right form "%s" already exists in language %s' % (right_comp, self.language))
                            continue
                        entry = sr_interface.vocabulary_entry(right_comp,
                            self.close_written_forms[i])
                        if sr_interface.word_exists(entry):
                            close_spoken_forms.append(right_comp)
                        else:
                            debug.trace('LeftWritePunctuation.create',
                                "word '%s' doesn't exist" % entry)
            if open_spoken_forms or close_spoken_forms:
                self._add_single(aliases, i, 
                    open_spoken_forms, close_spoken_forms)
                written = self.open_written_forms[i] + \
                    self.close_written_forms[i]
                plural = self.plural_spoken_forms[i]
                if plural:
                    self._add_empty(aliases, written,
                        plural, 
                        spacing = joins_identifier)
                    action = ActionInsert(code_bef = \
                        self.open_written_forms[i],
                        code_after = self.close_written_forms[i],
                        spacing = like_open_paren)
                    self._add_between(between, action,
                        self.singular_spoken_forms[i],
                        self.plural_spoken_forms[i])
                if not dictation_only:
                    self._add_single_navigation(navigation, i,
                        open_spoken_forms, close_spoken_forms)
                    self._add_either_navigation(navigation, i)
                    if plural:
                        self._add_jump_out(navigation,
                            self.open_written_forms[i],
                            self.close_written_forms[i],
                            self.plural_spoken_forms[i])
        interp.add_lsa_set(aliases)
        interp.add_csc_set(between)
        interp.add_csc_set(navigation)

    def _add_single(self, aliases, i, open_spoken_forms, 
        close_spoken_forms):
        """private method to add LSAs for dictation of left and right 
        elements of the pair of symbols
        
        **INPUTS**

        *LSAliasSet aliases* -- set to which to add the language-specific
        aliases 
        
        *INT i* -- index into list of written forms

        *[STR] open_spoken_forms* -- the spoken forms for the left/open
        symbol

        *[STR] close_spoken_forms* -- the spoken forms for the
        right/close symbol
        """
        aliases.add_lsa(LSAlias(open_spoken_forms, 
                        {self.language: self.open_written_forms[i]}, 
                        spacing = like_open_paren))
        aliases.add_lsa(LSAlias(close_spoken_forms, 
                        {self.language: self.close_written_forms[i]}, 
                        spacing = like_close_paren))

    def _add_single_navigation(self, commands, i, open_spoken_forms,
        close_spoken_forms):
        """private method to add CSCs for navigation by the left and
        right forms of this punctuation symbol
        
        **INPUTS**

        *CSCmdSet commands* -- set to which to add the context-sensitive
        commands

        *INT i* -- index into list of written forms

        *[STR] open_spoken_forms* -- the spoken forms for the left
        element of the pair

        *[STR] close_spoken_forms* -- the spoken forms for the right
        element of the pair
        """
        left_escaped = re.escape(self.open_written_forms[i])
        self._add_navigation(commands, expression = left_escaped,
            spoken_forms = open_spoken_forms) 
        right_escaped = re.escape(self.close_written_forms[i])
        self._add_navigation(commands, expression = right_escaped,
            spoken_forms = close_spoken_forms) 

    def _add_either_navigation(self, commands, i):
        """private method to add CSCs for navigation by either left or
        right forms of this punctuation symbol
        
        **INPUTS**

        *CSCmdSet commands* -- set to which to add the context-sensitive
        commands

        *INT i* -- index into list of written forms
        """
        left_escaped = re.escape(self.open_written_forms[i])
        right_escaped = re.escape(self.close_written_forms[i])
        expression = "[%s%s]" % (left_escaped, right_escaped)
        self._add_navigation(commands, expression,
            self.singular_spoken_forms[i]) 

class PairedQuotes(PairedPunctuation):
    """paired quotes with identical written forms for left/open vs. 
    right/close, used to define punctuation navigation commands and LSAs

    **INSTANCE ATTRIBUTES**

    *[STR]* written_forms -- written forms for each type of quote

    *[[STR]]* singular_spoken_forms -- corresponding (lists of) spoken forms 
    for single symbols (omitting open-/begin-/close-/end-)

    *[[STR]]* plural_spoken_forms -- corresponding spoken forms for 
    plural symbols

    *[BOOL]* no_empty -- corresponding flags indicating whether to omit 
    empty quotes form

    *[STR] open_prefixes* -- prefixes for open-/begin- member of the pair

    *[STR] close_prefixes* -- prefixes for close-/end- member of the pair
    """
    def __init__(self, **args):
        self.deep_construct(PairedQuotes,
                            {'written_forms': [],
                             'singular_spoken_forms': [],
                             'plural_spoken_forms': [],
                             'no_empty': [],
                             'no_singular_navigation': [],
                             'open_prefixes': None,
                             'close_prefixes': None},
                            args)
        self.set_prefixes()

    def set_prefixes(self, open_prefixes = None, close_prefixes = None):
        """set the prefixes for the open/begin and close/end spoken forms

        **INPUTS**

        *[STR] open_prefixes* -- prefixes for open/begin member of the pair, 
        or None for ['open-', 'begin-']

        *[STR] close_prefixes* -- prefixes for close/end member of the pair,
        or None for ['close-', 'end-']

        **OUTPUTS**

        *none*
        """
        if open_prefixes is None:
            open_prefixes = ['open-', 'begin-']
        if close_prefixes is None:
            close_prefixes = ['close-', 'end-']
        self.open_prefixes = open_prefixes
        self.close_prefixes = close_prefixes

    def add(self, written_form, singular_spoken_forms, 
            plural_spoken_forms, no_empty = 0, no_singular_navigation = 0):
        """add a type of quotes

        **INPUTS**

        *STR* written_form -- written form

        *[STR]* spoken_forms -- list of corresponding spoken forms 

        *BOOL* no_empty -- flag indicating whether to omit empty quotes form

        *BOOL* no_singular_navigation -- flag indicating whether to 
        omit commands for navigation around singular forms.  This is used 
        when adding paired quotes for a symbol which also has a standard
        SinglePunctuation.

        **OUTPUTS**

        *none*
        """
        self.written_forms.append(written_form)
        self.singular_spoken_forms.append(singular_spoken_forms)
        self.plural_spoken_forms.append(plural_spoken_forms)
        self.no_empty.append(no_empty)
        self.no_singular_navigation.append(no_singular_navigation)

    def create(self, interp, force = 0, dictation_only = 0):
        """add LSAs for dictation of punctuation symbols and CSCs for
        punctuation navigation
        
        **INPUTS**
        
        *CmdInterp interp* -- command interpreter (or NewMediatorObject,
        which will forward to the interpreter) to which to add the LSAs
        and CSCs

        *BOOL force* -- if true, create aliases even when another alias
        with the same spoken form exists (normally, for standard forms,
        we do not do this).

        *BOOL dictation_only* -- if true, add only LSAs for dictation 
        of punctuation symbols, but not punctuation navigation commands
        """
        if not interp:
            return
        aliases = LSAliasSet(self.name, 
            description = 'dictating open- and close- quotes')
        between = CSCmdSet("between " + self.name,
            description = 'dictating punctuation in matching pairs')
        navigation = CSCmdSet(self.name + " navigation", 
            description = 'navigation by punctuation')
        for i in range(len(self.written_forms)):
            open_spoken_forms = []
            close_spoken_forms = []
            for spoken in self.singular_spoken_forms[i]:
                for open_prefix in self.open_prefixes:
                    open_comp = "%s%s" % (open_prefix, spoken)
                    if force:
                        open_spoken_forms.append(open_comp)
                    else:
# only add the word if we don't already have an LSA with the same spoken
# form, and if the word exists in the vocabulary.  The latter prevents us 
# from accidentally adding back punctuation which the user has deleted from
# the vocabulary, or adding NaturallySpeaking US English spoken forms 
# for punctuation to the vocabulary of a different edition or different
# speech engine
                        if interp.has_lsa(open_comp, language =
                            self.language):
                            debug.trace('PairedQuotes.create',
                                'open form "%s" already exists in language %s' % (open_comp, self.language))
                            continue
                        entry = sr_interface.vocabulary_entry(open_comp,
                            self.written_forms[i])
                        if sr_interface.word_exists(entry):
                            open_spoken_forms.append(open_comp)
                        else:
                            debug.trace('PairedQuotes.create',
                                "word '%s' doesn't exist" % entry)
                for close_prefix in self.close_prefixes:
                    close_comp = "%s%s" % (close_prefix, spoken)
                    if force:
                        close_spoken_forms.append(close_comp)
                    else:
                        if interp.has_lsa(close_comp, language =
                            self.language):
                            debug.trace('PairedQuotes.create',
                                'close form "%s" already exists in language %s' % (open_comp, self.language))
                            continue
                        entry = sr_interface.vocabulary_entry(close_comp,
                            self.written_forms[i])
                        if sr_interface.word_exists(entry):
                            close_spoken_forms.append(close_comp)
                        else:
                            debug.trace('PairedQuotes.create',
                                "word '%s' doesn't exist" % entry)
            if open_spoken_forms or close_spoken_forms:
                self._add_single(aliases, i, 
                    open_spoken_forms, close_spoken_forms)
                plural = self.plural_spoken_forms[i]
                if plural:
                    if not self.no_empty[i]:
                        written = self.written_forms[i] * 2
                        self._add_empty(aliases, written,
                            plural, 
                            spacing = normal_spacing)
                    action = ActionInsert(code_bef = \
                        self.written_forms[i],
                        code_after = self.written_forms[i],
                        spacing = like_open_quote)
                    self._add_between(between, action,
                        self.singular_spoken_forms[i],
                        self.plural_spoken_forms[i])
                if not dictation_only:
                    expression = re.escape(self.written_forms[i])
                    if not self.no_singular_navigation[i]:
                        self._add_navigation(navigation, expression,
                            self.singular_spoken_forms[i])
                    if plural:
                        self._add_jump_out(navigation,
                            self.written_forms[i],
                            self.written_forms[i],
                            self.plural_spoken_forms[i])
        interp.add_lsa_set(aliases)
        interp.add_csc_set(between)
        interp.add_csc_set(navigation)


    def _add_single(self, aliases, i, open_spoken_forms, 
        close_spoken_forms):
        """private method to add LSAs for dictation of open and close
        quotes
        
        **INPUTS**

        *LSAliasSet aliases* -- set to which to add the language-specific
        aliases 
        
        *INT i* -- index into list of written forms

        *[STR] open_spoken_forms* -- the spoken forms for the left/open
        symbol

        *[STR] close_spoken_forms* -- the spoken forms for the
        right/close symbol
        """
        aliases.add_lsa(LSAlias(open_spoken_forms, 
                        {self.language: self.written_forms[i]}, 
                        spacing = like_open_quote))
        aliases.add_lsa(LSAlias(close_spoken_forms, 
                        {self.language: self.written_forms[i]}, 
                        spacing = like_close_quote))

 
 
class EnglishSmallNumbersSet(Object):
    """Class for generating LSAs for translating English 2-digit numbers.
    
    This is a stop-gap measure. Eventually, we will have a grammar that
    allows natural dictation of larger numbers. For now, the user will 
    have to dictate such numbers by uttering a sequence of 2-diti numbers 
    (e.g. say "twenty fifty" to type 2050).

    **INSTANCE ATTRIBUTES**

    [STR] *words_0_to_19* -- List of words used for numbers between 0 and 19.
    
    [STR] *words_multiples_of_10* -- List of words used for multiples of 10

    """
    def __init__(self, **args):
        debug.trace('EnglishSmallNumbersSet.__init__', 'invoked')        
        self.deep_construct(EnglishSmallNumbersSet,
                            {
                             'digits': ['zero', 'one', 'two', 
                                         'three', 'four', 'five', 'six',
                                         'seven', 'eight', 'nine'],
                             'words_0_19': ['zero', 'one', 'two', 
                                         'three', 'four', 'five', 'six',
                                         'seven', 'eight', 'nine',
                                         'ten', 'eleven', 'twelve',
                                         'thirteen', 'fourteen', 'fifteen', 
                                         'sixteen', 'seventeen',
                                         'eighteen', 'nineteen'],
                             'words_multiples_of_10': \
                                 ['ten', 'twenty', 'thirty', 'forty', 
                                  'fifty', 'sixty', 'seventy',
                                  'eighty', 'ninety']
                            }, args)
        debug.trace('EnglishSmallNumbersSet.__init__', 'exited')                              

                              
    def _add_number(self, aliases, number):        
        written = "%s" % number
        
        if number < 20:
           spoken = self.words_0_19[number]
        else:
           how_many_multiples_of_ten = int(number/10)
           tens_word = self.words_multiples_of_10[how_many_multiples_of_ten - 1]
           
           how_many_units = number - how_many_multiples_of_ten*10           
           if how_many_units > 0:
              units_word = "-%s" % self.words_0_19[how_many_units]
           else:
              units_word = ""
           
           spoken = "%s%s" % (tens_word, units_word)                             
                                    
                                                   
        aliases.add_lsa(LSAlias([spoken], 
                        {all_languages: written}, letters_and_digits, 
                        new_symbol = 'within'))

    def _add_zero_prefixed_numbers(self, aliases):
       aliases.add_lsa(LSAlias(['oh X.'], 
                       {all_languages: '0x'}, letters_and_digits))
    
       aliases.add_lsa(LSAlias(['thousand'], {all_languages: '000'},
           letters_and_digits, new_symbol = 'within'))
       aliases.add_lsa(LSAlias(['hundred'], {all_languages: '00'},
           letters_and_digits, new_symbol = 'within'))
       for digit in range(10):
          if digit == 0:
             spoken = 'oh oh'
             written = '00'
          else:
             spoken = 'oh %s' % self.words_0_19[digit]
             written = '0%s' % digit
          aliases.add_lsa(LSAlias([spoken], 
                          {all_languages: written}, letters_and_digits,
                          new_symbol = 'within'))
               
    def create(self, interp, numeral_prefix = None):
        """Add LSAs for dictation of English 2-digit numbers.
        
        **INPUTS**
        
        *CmdInterp interp* -- command interpreter (or NewMediatorObject,
        which will forward to the interpreter) to which to add the LSAs
        and CSCs

        *STR numeral_prefix* -- prefix to use for forms "numeral zero"
        through "numeral nine", or None or empty to omit prefixed forms
        """

        if not interp:
            return
            
        aliases = LSAliasSet('US small numbers', 
                             description = 'dictating 2-digit numbers.')                             
        
                                     
        for number in range(100):
            self._add_number(aliases, number)
           
        if numeral_prefix:
            for number in range(10):
                spoken = "%s%s" % (numeral_prefix, self.digits[number])
                written = "%d" % number
                aliases.add_lsa(LSAlias([spoken], 
                                {all_languages: written}, letters_and_digits, 
                                new_symbol = 'within'))

        self._add_zero_prefixed_numbers(aliases)
           
                   
        interp.add_lsa_set(aliases)
        

        
class StandardFunctionCallsHelper(Object):
    """Helper class for creating a CSC set for standard functions and 
    method names for a particular programming language.
    
    The standard functions sets are used to allow:
    
    - Distinction between a function name an possible homophonic variable 
      names.      
      
      For example in Python, the utterance "string with arguments" would 
      refer to the function str(), while the utterance "string" by itself 
      might refer to a variable "string".

    - Provide different written forms for the same function in different 
      languages. For example, the function "string with arguments" might
      translate to str() in Python but to string() in a different language.
      
    - Provide pronunciation for the standard function or method which 
      could not be automatically matched to its spoken form. For example, 
      you might want to say "absolute value with arguments" to type abs(), 
      even though the symbol matching algorithm does not allow 
      "absolute value" to match abs.    
      
    - Allow the user to employ the "of" suffix with standard function. 
      For example you might want "cosine of" to type "cos()". But you 
      wouldn't want to define a CSC "of" that types "()" because the word 
      "of" by itself is much too ambiguous. But the utterance "cosine of" 
      may be OK.            

    **INSTANCE ATTRIBUTES**

    *STR language* -- Name of the language that these functions belong to.
    
    *[STR] with_arguments_suffixes* -- List of suffixes that can be 
    appended to the function or method name to invoke it with a 
    non-empty arguments list (eg: in Python, that means: type () and put 
    the cursor IN BETWEEN).
    
    *[STR] without_arguments_suffixes* -- List of suffixes that can be 
    appended to the function or method name to invoke it with an empty 
    arguments list (eg: in Python, that means: type () and put the cursor 
    AFTER).
    
    *Action default_with_arguments_action* -- Action to be invoked
    to print a non-empty argument list for the function or method name.
    Can be overriden on a language specific basis through 
    *with_arguments_action*.
    
    *Action default_without_arguments_action* -- Action to be invoked
    to print a non-empty argument list for the function or method name.
    Can be overriden on a language specific basis through 
    *without_arguments_action*.
    
    *{STR: Action} with_arguments_actions* -- The key is a string defining 
    a programming language and the value is an Action to be invoked
    to print a non-empty argument list for the function or method name.
    
    *{STR: Action} without_arguments_actions* -- The key is a string 
    defining a programming language and the value is an Action to be 
    invoked to print an empty argument list for the function or method name.      
    """
    
    def __init__(self, language, 
                       with_arguments_suffixes =
                            ['with arguments', 'with argument', 'call with',
                            'called with', 'function of'], 
                       ambiguous_with_arguments_suffixes = ['of'],                            
                       without_arguments_suffixes =
                            ['with no arguments', 'without argument', 
                            'without arguments', 'with no argument', 
                            'empty function'], 
                       default_with_arguments_action=gen_parens_pair,
                       default_without_arguments_action=gen_empty_parens_pair,                       
                       with_arguments_actions={},
                       without_arguments_actions={},     
                       **args):
        self.deep_construct(Object,
            {'language': language,
             'with_arguments_suffixes': with_arguments_suffixes,
             'ambiguous_with_arguments_suffixes': \
                 ambiguous_with_arguments_suffixes,
             'without_arguments_suffixes': without_arguments_suffixes,
             'with_arguments_actions': with_arguments_actions,
             'without_arguments_actions': without_arguments_actions,
             'default_with_arguments_action': default_with_arguments_action,
             'default_without_arguments_action': \
                 default_without_arguments_action,
             'functions': {}},                             
            args)
                            
    def add_function_name(self, spoken_forms, written_form,  
                          forms = None, empty = 0):
        """Add a definition for a standard function or method name to 
        the set.
               
        **INPUTS**
        
        *[STR] spoken_forms -- List of spoken forms for the function. 
        
        *STR written_form* -- Written form of the function for language 
        *self.language*.
         
        *STR forms* -- indicates the type of forms to be used for
        non-empty calls.  

        Valid choices are
           'short': short forms only ("____ of")
           'long': for long forms only
           'all': for both
        
        If empty is true, we default to no non-empty forms,
        unless forms is specified explicitly.

        *BOOL empty* -- if true, then the function may be called with
        empty arguments
        """
        debug.trace('StandardFunctionCallsHelper.add_function_name', 
            'written_form = %s, empty = %d, forms = %s' \
            % (written_form, empty, forms))
        debug.trace('StandardFunctionCallsHelper.add_function_name', 
            'spoken_forms = %s' % spoken_forms)
        self.functions[written_form] = (spoken_forms, empty, forms)
       
    def create(self, interp):
        """
        *CmdInterp interp* -- command interpreter (or NewMediatorObject,
        which will forward to the interpreter) to which to add the LSAs
        and CSCs
        """
        if not interp:
           return    
        commands = CSCmdSet('%s standard functions' % (self.language),
                     description = 'common functions for %s' % self.language)

        for written_form in self.functions.keys():
            (spoken_forms, empty, forms) = self.functions[written_form]
            self._add_function_calls(commands, spoken_forms,
                written_form, forms, empty)
            self._add_function_name_symbol(spoken_forms, written_form, interp)

        interp.add_csc_set(commands)
       
    def _add_function_calls(self, commands, spoken_forms,
        written_form, forms = None, empty = 0):
        """Add CSCs for calling the function without any arguments
               
        **INPUTS**

        *CSCmdSet commands* -- command set to which to add the commands
        
        *[STR] spoken_forms -- List of spoken forms for the function. 
        
        *STR written_form* -- Written form of the function for 
        language *self.language*.
        
        *STR forms* -- indicates the type of forms to be used for
        non-empty calls.  

        Valid choices are
           'short': short forms only ("____ of")
           'long': for long forms only
           'all': for both

        If omitted, default to 'all', unless empty is true, in which
        case don't include any commands for non-empty calls

        *BOOL empty* -- if true, then include forms without arguments
        (and omit the rest unless forms is specified explicitly)
        """
        debug.trace('StandardFunctionCallsHelper._add_function_calls', 
            'written_form = %s, empty = %d, forms = %s' \
            % (written_form, empty, forms))
        debug.trace('StandardFunctionCallsHelper._add_function_calls', 
            'spoken_forms = %s' % spoken_forms)
        call_spoken_forms = []

        call_suffixes = []
        if forms is None and not empty:
            forms = 'all'
        if forms == 'short':
            call_suffixes = self.ambiguous_with_arguments_suffixes
        elif forms == 'long':
            call_suffixes = self.with_arguments_suffixes
        elif forms == 'all':
            call_suffixes = self.ambiguous_with_arguments_suffixes + \
                self.with_arguments_suffixes

        for a_spoken_form in spoken_forms:
            for a_suffix in call_suffixes:
                spoken_call = "%s %s" % (a_spoken_form, a_suffix)
                call_spoken_forms.append(spoken_call)
           
        call_action = self._function_call_action_factory(written_form, 
            'non-empty')
        cont_meanings = {ContNotAfterNewSymb(self.language): call_action}
           
        docstring = 'call %s function/method "%s"'  \
                     % (self.language, written_form)
        aCSC = CSCmd(spoken_forms = call_spoken_forms, 
                     meanings = cont_meanings,
                     docstring = docstring)
           
        debug.trace('StandardFunctionCallsHelper._add_function_calls', 
            'adding CSC for %s with spoken forms %s' %
            (written_form, call_spoken_forms))
        commands.add_csc(aCSC)

        if empty:
            call_action = self._function_call_action_factory(written_form, 
                'empty')

            call_spoken_forms = []

            for a_spoken_form in spoken_forms:
                for a_suffix in self.without_arguments_suffixes:
                    spoken_call = "%s %s" % (a_spoken_form, a_suffix)
                    call_spoken_forms.append(spoken_call)

            docstring = 'call %s function/method "%s"'  \
                     % (self.language, written_form)

            aCSC = CSCmd(spoken_forms = call_spoken_forms, 
                         meanings = cont_meanings,
                         docstring = docstring)
           
            commands.add_csc(aCSC, '%s without arguments' % spoken_forms[0])

    def _add_function_name_symbol(self, spoken_forms, written_form, interp):
        interp.add_symbol(written_form, spoken_forms, tentative=0)

    def _function_call_action_factory(self, func_name, args_list_type):
        """Returns an action for printing the function or method call."""
                    
        if args_list_type == 'non-empty-ambiguous':
            args_list_type = 'non-empty'
        return ActionFuncCallWithParens(func_name, args_list_type)

                            
    def _call_with_arguments_action(self, func_name, language): 
        if self.with_arguments_actions.has_key(language):
            return self.with_arguments_actions[language]
        else:
            return self.default_with_arguments_action

    def _call_without_arguments_action(self, func_name, language): 
        if self.without_arguments_actions.has_key(language):
            return self.without_arguments_actions[language]
        else:
            return self.default_without_arguments_action          
          
    
