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
import debug
import re
import string
import actions_gen

import CmdInterp, AppState


class GramCommon(Object):
    """abstract base class for all grammars

    **INSTANCE ATTRIBUTES**

    *BOOL* capitalize_rules -- if true, capitalize words in the rules.
    Ignored for dictation grammars
    """
    def __init__(self, capitalize_rules = 0, **args):
        self.deep_construct(GramCommon, {'capitalize_rules':
            capitalize_rules}, args)

    def capitalize_rule(self, rule):
        """adjusts case of rule, if necessary

        **INPUTS**

        *STR rule* -- the rule

        **OUTPUTS**

        *STR* -- the rule, with the proper case
        """
        if self.capitalize_rules:
            return string.capwords(rule)
        else:
            return string.lower(rule)


class WinGram(GramCommon, OwnerObject):
    """abstract base class for window-specific grammar interfaces

    **INSTANCE ATTRIBUTES**

    *AppState* manager -- the grammar manager which owns this grammar

    *BOOL* active -- is grammar active?

    *BOOL* exclusive -- is grammar exclusive?  (prevents other
    non-exclusive grammars from getting results)

    *INT* window -- window handle (unique identifier) for
    window-specific grammars (even if active, will only receive results
    when the corresponding window has the focus).

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, manager, window = None, exclusive = 0,
                 **attrs):
        self.deep_construct(WinGram,
            {
             'manager': manager, 'window' : window,
             'exclusive' : exclusive,
             'active' : 0
            }, attrs)
        self.name_parent('manager')

    def _set_exclusive_when_active(self, exclusive = 1):
        """private method which ensures that even currently active grammars 
        become exclusive.  This is important because activate may be 
        ignored if the grammar is already active, so a change to
        self.exclusive may not take effect even on the next utterance

        **INPUTS**

        *BOOL* exclusive -- true if the grammar should be exclusive

        **OUTPUTS**

        *none*
        """
        debug.virtual('WinGram._set_exclusive_when_active', self)
        
    def set_exclusive(self, exclusive = 1):
        """makes the grammar exclusive (or not).  Generally used only
        for background regression testing

        **INPUTS**

        *BOOL* exclusive -- true if the grammar should be exclusive

        **OUTPUTS**

        *none*
        """
        self.exclusive = exclusive
        if self.is_active():
            self._set_exclusive_when_active(exclusive)

    def activate(self):
        """activates the grammar for recognition
        tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('WinGram.activate')
    
    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('WinGram.deactivate')

    def window(self):
        """window to which the grammar is specific (or None for 
        a global grammar)

        **INPUTS**

        *none*
    
        **OUTPUTS**

        *INT* -- window handle (None indicates global)
        """
        return self.window

    def is_exclusive(self):
        """is the grammar exclusive?

        **INPUTS**

        *none*
    
        **OUTPUTS**

        *BOOL* -- exclusive?
        """
        return self.exclusive

    def is_active(self):
        """indicates whether the grammar is active for recognition 

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- returns true iff the grammar is active
        """
        return self.active

    def is_global(self):
        """tells whether the grammar is global

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is grammar set for global (window-independent)
        recognition.
        """
        return self.window == None

    def gram_type(self):
        """returns a subclass-dependent string describing the type of grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- type of grammar ('dictation', 'selection', or 'correction')
        """
        debug.virtual('WinGram.gram_type')
        
    def results_callback(self, utterance):
        """informs the GramMgr of the results of recognition
        
        Note: the sole purpose of this method is to provide feedback 
        to the user.  Any interpretation of the results and any action
        taken based on that interpretation must be handled separately.
        
        **INPUTS**

        *SpokenUtterance utterance* -- The utterance that got recognised.

        **OUTPUTS**

        *none*
        """
        words = utterance.words()
        debug.trace('WinGram.results_callback', 'words=%s' % repr(words))
        s = self.format_utterance_message(words)
        if self.manager:
            self.manager.user_message(s)

    def format_utterance_message(self, words):
        """formats the recognition results into a user message
        
        **INPUTS**

        *[(STR, STR)]* words* -- list of spoken, written forms 
        representing the recognition results

        **OUTPUTS**

        *STR* -- the message
        """
        s = "Heard %s" % (string.join(map(lambda x: x[0], words)))
        if debug.tracing('WinGram.format_utterance_message'):
            debug.trace('WinGram.format_utterance_message', 
                "Full utterance was %s" % repr(words))
        return s

        
class DictWinGram(WinGram):
    """abstract base class for window-specific dictation grammar interfaces

    **INSTANCE ATTRIBUTES**

    *AppState* app -- application which is the target of the grammar

    *STR* buff_name -- name of the buffer corresponding to this
    grammar.  Buff_name will be passed to
    CmdInterp.interpret_NL_cmd as the initial buffer.

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, app, buff_name = None, **attrs):
        self.deep_construct(DictWinGram,
            {'app': app,'buff_name': buff_name}, attrs)

    def gram_type(self):
        """returns a subclass-dependent string describing the type of grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- type of grammar ('dictation', 'selection', or 'correction')
        """
        return 'dictation'
        
    def interpreter(self):
        """return a reference to the mediator's current CmdInterp object

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.manager.interpreter()

    def rename_buffer_cbk(self, new_buff_name):
        """callback from GramMgr which notifies us that the application
        has renamed buffer corresponding to this dictation grammar

        **INPUTS**

        *STR* new_buff_name -- new name of the buffer 

        *none*
        """
        debug.trace('DictWinGram.rename_buffer_cbk', 'new_buff_name=%s' % new_buff_name)
        self.buff_name = new_buff_name

    def set_context(self, before = "", after = ""):
        """set the context to improve dictation accuracy

        **INPUTS**

        *STR* before -- one or more words said immediately before the
        next utterance, or found in the text immediately before the
        utterance

        *STR* after -- one or more words found in the text
        immediately after the utterance 

        **OUTPUTS**

        *none*
        """
        debug.virtual('DictWinGram.set_context')

    def on_results(self, results):
        """interpret the results of recognition.  This method must be
        called by the concrete subclass of DictWinGram.

        **INPUTS**

        *SpokenUtterance results* -- the SpokenUtterance object
        representing the recognition results

        **OUTPUTS**

        *none*
        """
        if debug.tracing('DictWinGram.on_results'):
            debug.trace('DictWinGram.on_results', 'results.words()=%s, self.results_callback=%s, self.manager=%s' % 
                        (repr(results.words()), self.results_callback, self.manager))
        self.results_callback(results)
        self.manager.interpret_dictation(results, \
            initial_buffer = self.buff_name)


class SelectWinGram(WinGram):
    """abstract base class for selection grammar interfaces

    **INSTANCE ATTRIBUTES**

    *AppState* app -- application to which results will be sent
    
    *STR* buff_name -- name of buffer to which to tie this 
    selection grammar (can also be set by activate)

    *[STR]* select_phrases -- words (or phrases) which introduces a
    selection utterance

    *STR* through_word -- word which separates the beginning and end of
    a range to be selected.

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, app, buff_name = None, select_phrases = ['select'],
        through_word = 'through', **attrs):
        """
        **INPUTS**

        *AppState* app -- application to which results will be sent
        
        *STR* buff_name -- name of buffer to which to tie this 
        selection grammar (can also be set by activate)
        """
        self.deep_construct(SelectWinGram,
            {'app': app, 'buff_name' : buff_name, 
            'select_phrases' : None, 'through_word' : None}, 
            attrs)
        self.select_phrases = map(self.capitalize_rule, select_phrases)
        self.through_word = self.capitalize_rule(through_word)

    def remove_other_references(self):
# not necessary, but can't hurt
        self.app = None

    def gram_type(self):
        """returns a subclass-dependent string describing the type of grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- type of grammar ('dictation', 'selection', or 'correction')
        """
        return 'selection'
        

    def _set_visible(self, visible):
        """internal call to set the currently visible range.

        **INPUTS**

        *STR* visible -- visible text range 

        **OUTPUTS**

        *none*
        """
        debug.virtual('SelectWinGram._set_visible')

    def find_visible(self):
        """find the currently visible range for self.buff_name
        and checks with buffer for the currently visible range.

        **INPUTS**

        *STR* buff_name -- name of currently active buffer

        **OUTPUTS**

        *none*
        """
        buff_name = self.buff_name
        vis_start, vis_end = self.app.get_visible(buff_name = buff_name)
        self.vis_start = vis_start
        visible = \
            self.app.get_text(vis_start, vis_end, buff_name = buff_name)
        self._set_visible(visible)

    def activate(self, buff_name):
        """activates the grammar for recognition tied to the current window,
        and checks with buffer for the currently visible range.

        **INPUTS**

        *STR* buff_name -- name of currently active buffer

        **OUTPUTS**

        *none*
        """
        debug.virtual('SelectWinGram.activate')


    def buffer_name(self):
        """returns name of buffer corresponding to this selection grammar.

        **INPUTS**

        *none*
    
        **OUTPUTS**

        *STR* -- name of buffer currently used by this selection grammar.
        """

        return self.buff_name
    
    def find_closest(self, verb, spoken_form, ranges):
        """Sort the ranges from earliest to latest, and select the one
        which is closest to the cursor in the proper direction

        **INPUTS**

        *STR* verb -- verb used by the selection

        *STR* spoken_form -- The spoken form of the selected code.

        *[(INT, INT)] -- list of ranges of offsets into buffer with the
        best recognition score
        """
        
        #
        # Analyse the verb used by the user in the Select utterance
        #
        debug.trace('SelectWinGram.find_closest', 'verb=%s, spoken_form=%s, ranges=%s' % 
                                                   (verb, spoken_form, repr(ranges)))
        direction = None
        if re.search('previous', verb, re.IGNORECASE):
            direction = -1
        if re.search('next', verb, re.IGNORECASE):                
            direction = 1

        mark_selection = 1
        if re.search('go', verb, re.IGNORECASE) or \
                re.search('before', verb, re.IGNORECASE) or \
                re.search('after', verb, re.IGNORECASE):
            mark_selection = 0

        where = 1
        if re.search('before', verb, re.IGNORECASE):
            where = -1
        if re.search('after', verb, re.IGNORECASE):
            where = 1


        debug.trace('SelectWinGram.find_closest', 'direction=%s, where=%s' % (direction, where))
 
        ranges.sort()
        closest_range_index = \
            self.app.closest_occurence_to_cursor(ranges, 
                regexp=spoken_form, 
                direction=direction, where=where,
                ignore_overlapping_with_cursor=0,
                buff_name = self.buff_name)
                
        if debug.tracing('SelectWinGram.find_closest'):
            debug.trace('SelectWinGram.find_closest', '** ranges=%s, closest_range_index=%s' % (repr(ranges), closest_range_index))
        if closest_range_index == None:
            return

        #
        # Need to figure out in which direction we are moving in initially
        #           
        selected_range = ranges[closest_range_index]
        if where > 0:
           move_curs_to_pos = selected_range[1]
        else:
           move_curs_to_pos = selected_range[0]
        if self.app.cur_pos() > move_curs_to_pos:
           direction = -1
        else:
           direction = 1
        debug.trace('SelectWinGram.find_closest', '** move_curs_to_pos=%s, self.app.cur_pos()=%s' % 
                    (move_curs_to_pos, self.app.cur_pos()))

        #
        # Mark selection and/or move cursor  to the appropriate end of
        # the selection.
        #
        debug.trace('SelectWinGram.find_closest', 
            '** mark_selection=%s' % mark_selection)
        a = actions_gen.ActionNavigateByPseudoCode(possible_ranges = ranges, 
            select_range_no = closest_range_index,
            buff_name = self.buff_name, cursor_at=where, 
            mark_selection=mark_selection, direction=direction)
        a.log_execute(self.app, None)

# this is needed for the EdSim mediator simulator.  We want EdSim to
# refresh at the end of interpretation of a whole utterance, not with 
# every change to the buffer.  Other editors will usually refresh
# instantly and automatically, so their AppState/SourceBuff
# implementations can simply ignore the print_buff_if_necessary message.

        self.app.print_buff_if_necessary(buff_name = self.buff_name)

        #
        # Log the selected occurence so that if the user repeats the
        # same Select Pseudocode operation we don't end up selecting
        # the same occurence again
        #
        self.app.log_search(regexp=spoken_form, 
            direction=direction, where=where, 
            match=ranges[closest_range_index],
            buff_name = self.buff_name)


class BasicCorrectionWinGram(WinGram):
    """abstract base class for window-specific grammar for basic
    correction (Scratch That/n, Correct That, Correct Recent)

    **INSTANCE ATTRIBUTES**

    *[STR]* scratch_words -- list of synonyms for Scratch in "Scratch
    That" and "Scratch n"

    *[STR]* correct_words -- list of synonyms for Correct in "Correct
    That" and "Correct Recent" 

    *[STR]* recent_words -- list of synonyms for Recent in "Correct Recent"

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, scratch_words = None, correct_words = None,
        recent_words = None, **attrs):
        """
        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which owns this grammar

        *[STR]* scratch_words -- list of synonyms for Scratch in "Scratch
        That" and "Scratch Last n", or None for the default of 'Scratch'

        *[STR]* correct_words -- list of synonyms for Correct in "Correct
        That" and "Correct Recent"  or None for the default of 'Correct'

        *[STR]* recent_words -- list of synonyms for 
        Recent in "Correct Recent" or None for the dfault of 'Recent
        """
        self.deep_construct(BasicCorrectionWinGram,
            {'scratch_words': None,
             'correct_words': None,
             'recent_words': None
            }, attrs)
        if scratch_words is None:
            scratch_words = ['Scratch', 'Undo']
        if correct_words is None:
            correct_words = ['Correct']
        if recent_words is None:
            recent_words = ['Recent']
        self.scratch_words = map(self.capitalize_rule, scratch_words)
        self.correct_words = map(self.capitalize_rule, correct_words)
        self.recent_words = map(self.capitalize_rule, recent_words)

    def gram_type(self):
        """returns a subclass-dependent string describing the type of grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- type of grammar ('dictation', 'selection', or 'correction')
        """
        return 'correction'
        
    def scratch_recent(self, n = 1):
        """method which subclass must call when the grammar 
        recognizes Scratch That/Scratch n, to undo the effect of the 
        most recent n utterances, if possible.

        **INPUTS**

        *INT n* -- number of utterances to undo

        **OUTPUTS**
        
        *INT* -- number of utterances successfully undone
        """
        debug.trace('BasicCorrectionWinGram.scratch_recent', 
            'trying to scratch %d' %n)
        n_done = self.manager.scratch_recent(n)
        debug.trace('BasicCorrectionWinGram.scratch_recent', 
            'actually scratched %d' %n_done)
        return n_done


    def on_correct_last(self):
        """method which subclass must call when the grammar 
        recognizes Correct That with nothing selected

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.manager.correct_last()

    def on_correct_recent(self):
        """method which subclass must call when the grammar 
        recognizes Correct Recent 

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.manager.correct_recent()


class SymbolReformattingWinGram(WinGram):
    """abstract base class for window-specific grammar for reformatting
    symbols.

    **INSTANCE ATTRIBUTES**


    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, reformat_words = None, recent_words = None, **attrs):
        """
        **INPUTS**

        """
        self.deep_construct(SymbolReformattingWinGram,
            {'reformat_words': reformat_words, 'recent_words': recent_words}, 
            attrs)            
        if reformat_words is None:
            reformat_words = ['Reformat']
        if recent_words is None:
            recent_words = ['Recent', 'Symbol', 'Symbols']
        self.reformat_words = map(self.capitalize_rule, reformat_words)
        self.recent_words = map(self.capitalize_rule, recent_words)
            

    def gram_type(self):
        """returns a subclass-dependent string describing the type of grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- type of grammar ('dictation', 'selection', 'correction', or 'reformatting')
        """
        return 'reformatting'

    def on_reformat_recent(self):
        """method which subclass must call when the grammar 
        recognizes Reformat Recent 

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.manager.reformat_recent()


class DiscreteCSCsAndLSAsWinGram(WinGram):
    """abstract base class for window-specific grammar for recognising
       discrete LSAs and CSCs.

    **INSTANCE ATTRIBUTES**


    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **attrs):
        """
        **INPUTS**

        """
        self.deep_construct(DiscreteCSCsAndLSAsWinGram,
            {}, 
            attrs)                        

    def gram_type(self):
        """returns a subclass-dependent string describing the type of grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- type of grammar ('dictation', 'selection', 'correction', or 'reformatting')
        """
        return 'discrete_lsa_csc'


class WinGramFactory(Object):
    """abstract base class for a factory which returns 
    window-specific grammars

    **INSTANCE ATTRIBUTES**

    [STR] *select_verbs = ['go', 'select', 'insert', 'correct', '']* -- list 
    of verbs to specify the action to be done by a SelectPseudocode utterance.

    [STR] *select_cursor_position_words = ['before', 'after', '']* -- list of words that can be 
    used to specify the position of the cursor in a SelectPseudocode utterance.
    
    [STR] *select_direction_words = ['next', 'previous', '']* -- list of words that
    can be used to specify the direction in which the search will be done
    for a SelectPseudocode utterance.

    STR *through_word* -- word for selecting a range with the 
    selection grammars

    [STR] *scratch_words* -- list of synonyms for Scratch in "Scratch
    That" and "Scratch n" in basic correction grammars

    [STR] *correct_words* -- list of synonyms for Correct in "Correct
    That" and "Correct Recent"  in basic correction grammars

    [STR] *recent_words* -- *[STR]* correct_words -- list of synonyms for 
    Recent in "Correct Recent" in basic correction grammars

    *BOOL* capitalize_rules -- if true, capitalize words in the rules
    for all command grammars.  Ignored for dictation grammars.

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, 
#        select_verbs = ['go', 'select', 'insert', 'correct', ''], 
# Look, I commented out correct for a reason - it is not supported
# without correct-anywhere -- DCF
        select_verbs = ['select'], 
        select_cursor_position_words = ['go before', 'go after', 
            'insert before', 'insert after', 'before', 'after'],
        select_direction_words = ['next', 'previous'],
        through_word = 'through',
        scratch_words = None,
        correct_words = None,
        recent_words = None,
        capitalize_rules = 0,
        reformat_words = None,
        **attrs):
        """
        **INPUTS**

        [STR] *select_verbs = ['go', 'select', 'insert', 'correct']* -- list 
        of verbs to specify the action to be done by a SelectPseudocode utterance.

        [STR] *select_cursor_position_words = ['before', 'after']* -- list of words that can be 
        used to specify the position of the cursor in a SelectPseudocode utterance.
    
        [STR] *select_direction_words = ['next', 'previous']* -- list of words that
        can be used to specify the direction in which the search will be done
        for a SelectPseudocode utterance.


        *STR* through_word -- word for selecting a range with the 
        selection grammars

        *[STR]* scratch_words -- list of synonyms for Scratch in "Scratch
        That" and "Scratch n", or None for the default of 'Scratch'

        *[STR]* correct_words -- list of synonyms for Correct in "Correct
        That" and "Correct Recent"  or None for the default of 'Correct'

        *[STR]* recent_words -- list of synonyms for 
        Recent in "Correct Recent" or None for the dfault of 'Recent

        **OUTPUTS**

        *none*
        """
        self.deep_construct(WinGramFactory,
            {'select_verbs': select_verbs,

             'select_cursor_position_words': select_cursor_position_words,
             'select_direction_words': select_direction_words,
             'through_word' : through_word,
             'scratch_words': scratch_words,
             'correct_words': correct_words,
             'recent_words': recent_words,
             'capitalize': capitalize_rules,
             'reformat_words': reformat_words}, 
            attrs)

    def capitalize_rules(self, capitalize):
        """specifies whether words in rules for context-free grammars 
        should be capitalized.
        
        Note: This is important for ensuring that the correction grammar
        overrides the corresponding built-in grammars.  capitalize_rules
        should be true for NaturallySpeaking 5 or earlier, but false for
        NaturallySpeaking 6 or later (have to check about v. 5)

        **INPUTS**

        *BOOL* capitalize -- if true, then words in rules like "scratch
        that" should be capitalized.

        **OUTPUTS**

        *none*
        """
        self.capitalize = capitalize

    def make_dictation(self, manager, app, buff_name, window = None,
        exclusive = 0):
        """create a new dictation grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the new
        grammar

        *AppState* app -- application which is the target of the grammar

        *STR* buff_name -- name of the buffer corresponding to this
        grammar.  Buff_name will be passed to
        CmdInterp.interpret_NL_cmd as the initial buffer.

        *INT* window -- make grammar specific to a particular window
        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)
        
        **OUTPUTS**

        *DictWinGram* -- new dictation grammar
        """
        debug.virtual('WinGramFactory.make_dictation')
    
    def make_dictation_through_cmd(self, manager, app, buff_name, window = None,
        exclusive = 0):
        """create a new grammar that recognizes utterances that start and/or
        end with known spoken forms (known symbols, CSCs or LSAs).

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the
        grammar

        *AppState* app -- application which is the target of the grammar

        *STR* buff_name -- name of the buffer corresponding to this
        grammar.  Buff_name will be passed to
        CmdInterp.interpret_NL_cmd as the initial buffer.

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)
        
        **OUTPUTS**

        *DictationGramSetNL* -- new pair of grammars (dictation plus command)
        for supporting VoiceCode dictation.
        """
        debug.virtual('WinGramFactory.make_dictation_through_cmd')
    
    def make_selection(self, manager, app, window = None, buff_name = None,
        exclusive = 0):
        """create a new selection grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the new
        grammar

        *AppState* app -- application corresponding to the selection
        grammar, which is queried with buff_name for the currently
        visible range, and is notified of selection changes

        *STR* buff_name -- name of the buffer corresponding to this
        grammar.  Can also be set later in the activate call to the
        grammar.

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *SelectWinGram* -- new selection grammar
        """
        debug.virtual('WinGramFactory.make_selection')

    def _select_phrases(self):
       """Generates phrases that can be used to select a portion of code."""
       phrases = []
       for verb in self.select_verbs:
           phrases.append(verb)
           for direction in self.select_direction_words:
               this_phrase = '%s %s' % (verb, direction)
               phrases.append(this_phrase)
       for put_cursor in self.select_cursor_position_words:
           phrases.append(put_cursor)
           for direction in self.select_direction_words:
               this_phrase = '%s %s' % (put_cursor, direction)
               phrases.append(this_phrase)
                
       debug.trace('WinGramFactory._select_phrases', 'returning phrases=%s' % repr(phrases))
       return phrases

    def make_correction(self, manager, window = None, exclusive = 0):
        """create a new basic correction grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which owns this grammar

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *BasicCorrectionWinGram* -- new basic correction grammar
        """
        debug.virtual('WinGramFactory.make_correction')

    def make_reformatting(self, manager, window = None, exclusive = 0):
        """create a new symbol reformatting grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which owns this grammar

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *SymbolReformattingWinGram* -- new symbol reformatting grammar
        """
        debug.virtual('WinGramFactory.make_reformatting')


    def make_choices(self, choice_words):
        """create a new ChoiceGram choice grammar

        **INPUTS**

        *[STR]* choice_words -- grammar will be <choice_words> 1toN

        **OUTPUTS**

        *ChoiceGram* -- new choice grammar
        """
        debug.virtual('WinGramFactory.make_choices')

    def make_natural_spelling(self, spell_words = None, spelling_cbk = None):
        """create a new NaturalSpelling grammar

        **INPUTS**

        *[STR]* spell_words -- words which must precede the first spelled 
        letter, or None for an unrestricted spelling grammar.  The latter is not
        advisable unless dictation is disabled.

        *FCT(STR)* spelling_cbk -- callback to signal recognition.
        Currently, the letters or numbers spelled are returned as a single 
        string (with double-o, etc. expanded)

        **OUTPUTS**

        *NaturalSpelling* -- the spelling grammar
        """
        debug.virtual('WinGramFactory.make_natural_spelling')

    def make_military_spelling(self, spell_words = None, spelling_cbk = None):
        """create a new MilitarySpelling grammar

        **INPUTS**

        *[STR]* spell_words -- words which must precede the first spelled 
        letter, or None for an unrestricted spelling grammar.  The latter is not
        advisable unless dictation is disabled.

        *FCT(STR)* spelling_cbk -- callback to signal recognition.
        Currently, the letters or numbers spelled are returned as a single 
        string 

        **OUTPUTS**

        *MilitarySpelling* -- the spelling grammar
        """
        debug.virtual('WinGramFactory.make_military_spelling')

    def make_simple_selection(self, get_visible_cbk, get_selection_cbk, 
        select_cbk, alt_select_phrases = None):
        """create a new SimpleSelection grammar

        **INPUTS**

        *STR FCT() get_visible_cbk* -- callback for retrieving the visible range

        *(INT, INT) FCT() get_selection_cbk* -- callback for retrieving the 
        current selection

        *FCT(range) select_cbk* -- callback which returns the *(INT, INT)* 
        range to be selected (relative to the start of the visible range 
        passed to the get_visible_cbk), or None if text wasn't found

        *[STR]* alt_select_phrases -- words (or phrases) which introduces a
        selection utterance, or None to use the same value as
        make_selection does.  (Warning: once we add correct xyz, this
        won't be wise any more).

        **OUTPUTS**

        *SimpleSelection* -- new selection grammar
        """
        debug.virtual('WinGramFactory.make_simple_selection')
        
    def make_text_mode(self, window):
        """Create a new grammar for toggling text-mode on and off.
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *TextModeGram* -- the command grammar for toggling text-mode.
        
        """
        debug.virtual('WinGramFactory.make_text_mode')

        
    
class DictWinGramDummy(DictWinGram):
    """dummy implementation of window-specific dictation grammar 

    **INSTANCE ATTRIBUTES**

    *BOOL* silent -- don't print diagnostics

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, silent = 0, **attrs):
        self.deep_construct(DictWinGramDummy,
            {'silent': silent}, attrs)
        if not self.silent:
            self.identify_grammar()
            print "init"

    def __del__(self):
        if not self.silent:
            self.identify_grammar()
            print "del"

    def identify_grammar(self):
        """print information identifying the grammar by buffer and
        window

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.window == None:
            winname = "global"
        else:
            winname = "window %d" % self.window
        print "DictWinGramDummy for buffer = %s, %s" % \
            (repr(self.buff_name), winname)

    def set_context(self, before = "", after = ""):
        """set the context to improve dictation accuracy

        **INPUTS**

        *STR* before -- one or more words said immediately before the
        next utterance, or found in the text immediately before the
        utterance

        *STR* after -- one or more words found in the text
        immediately after the utterance 

        **OUTPUTS**

        *none*
        """
        if not self.silent:
            self.identify_grammar()
            print "setting context: before = [%s], after = [%s]" % (before,
                after)

    def activate(self):
        """activates the grammar for recognition
        tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if not self.silent:
            self.identify_grammar()
            print "activating: ",
            if self.window == None:
                print "global ",
            else:
                print "%d " % (self.window),
            if self.exclusive:
                print "exclusive "
            print ""
        self.active = 1
    
    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if not self.silent:
            self.identify_grammar()
            print "deactivating"
        self.active = 0

class DictThroughCmdGramDummy(DictWinGramDummy):
    """dummy implementation of window-specific dictation through
    commands grammar 

    **INSTANCE ATTRIBUTES**

    *BOOL* silent -- don't print diagnostics

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, silent = 0, **attrs):
        self.deep_construct(DictThroughCmdGramDummy,
            {'silent': silent}, attrs)
        if not self.silent:
            self.identify_grammar()
            print "init"

    def identify_grammar(self):
        """print information identifying the grammar by buffer and
        window

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.window == None:
            winname = "global"
        else:
            winname = "window %d" % self.window
        print "DictThroughCmdGramDummy for buffer = %s, %s" % \
            (repr(self.buff_name), winname)



class SelectWinGramDummy(SelectWinGram):
    """dummy implementation of window-specific selection grammar 

    **INSTANCE ATTRIBUTES**

    *BOOL* silent -- don't print diagnostics

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, silent = 0, **attrs):
        """
        **INPUTS**

        *none*
        """
        self.deep_construct(SelectWinGramDummy,
            {'silent': silent}, attrs)
        if not self.silent:
            self.identify_grammar()
            print "init"

    def __del__(self):
        if not self.silent:
            self.identify_grammar()
            print "del"

    def identify_grammar(self):
        """print information identifying the grammar by buffer and
        window

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.window == None:
            winname = "global"
        else:
            winname = "window %d" % self.window
        print "SelectWinGramDummy for buffer %s, %s" % \
            (repr(self.buff_name), winname)

    def activate(self, buff_name):
        """activates the grammar for recognition tied to the current window,
        and checks with buffer for the currently visible range.

        **INPUTS**

        *STR* buff_name -- name of currently active buffer

        **OUTPUTS**

        *none*
        """
        debug.trace('SelectWinGramDummy.activate', 'buff_name=%s' % buff_name)
        self.buff_name = buff_name
        self.active = 1
        if not self.silent:
            self.identify_grammar()
            print "activating: ",
            if self.window == None:
                print "global ",
            else:
                print "%d " % (self.window),
            if self.exclusive:
                print "exclusive "
            print ""
    
    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if not self.silent:
            self.identify_grammar()
            print "deactivating"
        self.active = 0

class BasicCorrectionWinGramDummy(BasicCorrectionWinGram):
    """dummy implementation of window-specific basic correction grammar 

    **INSTANCE ATTRIBUTES**

    *BOOL* silent -- don't print diagnostics

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, silent = 0, **attrs):
        self.deep_construct(BasicCorrectionWinGramDummy,
            {'silent': silent}, attrs)
        if not self.silent:
            self.identify_grammar()
            print "init"

    def __del__(self):
        if not self.silent:
            self.identify_grammar()
            print "del"

    def identify_grammar(self):
        """print information identifying the grammar by buffer and
        window

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.window == None:
            winname = "global"
        else:
            winname = "window %d" % self.window
        print "BasicCorrectionWinGramDummy for window = %s" % winname

    def activate(self):
        """activates the grammar for recognition
        tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if not self.silent:
            self.identify_grammar()
            print "activating: ",
            if self.window == None:
                print "global ",
            else:
                print "%d " % (self.window),
            if self.exclusive:
                print "exclusive "
            print ""
        self.active = 1
    
    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if not self.silent:
            self.identify_grammar()
            print "deactivating"
        self.active = 0


class SymbolReformattingWinGramDummy(SymbolReformattingWinGram):
    """dummy implementation of window-specific symbol reformatting grammar 

    **INSTANCE ATTRIBUTES**

    *BOOL* silent -- don't print diagnostics

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, silent = 0, **attrs):
        self.deep_construct(SymbolReformattingWinGramDummy,
            {'silent': silent}, attrs)
        if not self.silent:
            self.identify_grammar()
            print "init"

    def __del__(self):
        if not self.silent:
            self.identify_grammar()
            print "del"

    def identify_grammar(self):
        """print information identifying the grammar by buffer and
        window

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.window == None:
            winname = "global"
        else:
            winname = "window %d" % self.window
        print "SymbolReformattingWinGramDummy for window = %s" % winname

    def activate(self):
        """activates the grammar for recognition
        tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if not self.silent:
            self.identify_grammar()
            print "activating: ",
            if self.window == None:
                print "global ",
            else:
                print "%d " % (self.window),
            if self.exclusive:
                print "exclusive "
            print ""
        self.active = 1
    
    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if not self.silent:
            self.identify_grammar()
            print "deactivating"
        self.active = 0


class WinGramFactoryDummy(Object):
    """implementation fo WinGramFactory with dummy grammars for
    regression testing.

    **INSTANCE ATTRIBUTES**

    *BOOL* silent -- don't print diagnostics

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, silent = 0, **attrs):
        """
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.deep_construct(WinGramFactoryDummy,
            {}, attrs)

    def make_dictation(self, manager, app, buff_name, window = None,
        exclusive = 0):
        """create a new dictation grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the new
        grammar

        *AppState* app -- application to which to forward results

        *STR* buff_name -- name of the buffer corresponding to this
        grammar.  Buff_name will be passed to
        CmdInterp.interpret_NL_cmd as the initial buffer.

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *DictWinGram* -- new dictation grammar
        """
        return DictWinGramDummy(manager = manager, app = app, 
            buff_name = buff_name, window = window, exclusive =
            exclusive)
                        
    
    def make_dictation_through_cmd(self, manager, app, buff_name, window = None,
        exclusive = 0):
        """create a new grammar that recognizes utterances that start and/or
        end with known spoken forms (known symbols, CSCs or LSAs).

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the
        grammar

        *AppState* app -- application which is the target of the grammar

        *STR* buff_name -- name of the buffer corresponding to this
        grammar.  Buff_name will be passed to
        CmdInterp.interpret_NL_cmd as the initial buffer.

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)
        
        **OUTPUTS**

        *DictationGramSetNL* -- new pair of grammars (dictation plus command)
        for supporting VoiceCode dictation.
        """
        return DictThroughCmdGramDummy(manager = manager, app = app, 
            buff_name = buff_name, window = window, exclusive =
            exclusive) 
    
    def make_selection(self, manager, app, window = None, buff_name = None,
        exclusive = 0):
        """create a new selection grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the new
        grammar

        *AppState* app -- application corresponding to the selection
        grammar, which is queried with buff_name for the currently
        visible range, and is notified of selection changes

        *STR* buff_name -- name of the buffer corresponding to this
        grammar.  Can also be set later in the activate call to the
        grammar.

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *SelectWinGram* -- new selection grammar
        """
        return SelectWinGramDummy(manager = manager, app = app, 
            buff_name = buff_name, 
            window = window, exclusive = exclusive)

    def make_correction(self, manager, window = None, exclusive = 0):
        """create a new basic correction grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which owns this grammar

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *BasicCorrectionWinGram* -- new basic correction grammar
        """
        return BasicCorrectionWinGramDummy(manager = manager, window = window, 
            exclusive = exclusive)
    
    def make_reformatting(self, manager, window = None, exclusive = 0):
        """create a new symbol reformatting grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which owns this grammar

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *SymbolReformattingWinGram* -- new basic correction grammar
        """
        return SymbolReformattingWinGramDummy(manager = manager, window = window, 
            exclusive = exclusive)
    
class ChoiceGram(GramCommon):
    """abstract base class for correction window Choose n grammar

    **INSTANCE ATTRIBUTES**

    *BOOL* active -- is grammar active?

    *[STR]* choice_words -- grammar will be <choice_words> 1toN
    """
    def __init__(self, choice_words, **attrs):
        self.deep_construct(ChoiceGram,
            {'active' : 0,
             'choice_words': None
            }, attrs)
        self.choice_words = map(self.capitalize_rule, choice_words)

    def activate(self, n, window, choice_cbk):
        """activates the grammar for recognition tied to a window
        with the given handle

        **INPUTS**

        *INT n* -- the maximum choice number

        *INT* window -- window handle (unique identifier) for the window

        *FCT(INT)* choice_cbk -- callback to signal recognition

        **OUTPUTS**

        *none*
        """
        debug.virtual('ChoiceGram.activate')

    def cleanup(self):
        """method which must be called by the owner prior to deleting
        the grammar, to ensure that it doesn't have circular references
        to the owner
        """
        debug.virtual('ChoiceGram.cleanup')

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('ChoiceGram.deactivate')

class NaturalSpelling(GramCommon):
    """abstract base class for natural spelling grammar (i.e. a b c, not
    alpha bravo charlie)

    **INSTANCE ATTRIBUTES**

    *BOOL* active -- is grammar active?

    *[STR]* spell_words -- words which must precede the first spelled letter, 
    or None for an unrestricted spelling grammar.  The latter is not
    advisable unless dictation is disabled.

    *FCT(STR)* spelling_cbk -- callback to signal recognition.
    Currently, the letters or numbers spelled are returned as a single string
    (with double-o, etc. expanded)
    """
    def __init__(self, spell_words = None, spelling_cbk = None, **attrs):
        self.deep_construct(NaturalSpelling,
            {'active' : 0,
             'spell_words': spell_words,
             'spelling_cbk': spelling_cbk
            }, attrs)

        if spell_words:
            self.spell_words = map(self.capitalize_rule, spell_words)

    def activate(self, window):
        """activates the grammar for recognition tied to a window
        with the given handle

        **INPUTS**

        *INT* window -- window handle (unique identifier) for the window

        **OUTPUTS**

        *none*
        """
        debug.virtual('NaturalSpelling.activate')

    def cleanup(self):
        """method which must be called by the owner prior to deleting
        the grammar, to ensure that it doesn't have circular references
        to the owner
        """
        self.spelling_cbk = None

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('NaturalSpelling.deactivate')

class MilitarySpelling(GramCommon):
    """abstract base class for military (alpha-bravo-charlie) spelling grammar 

    **INSTANCE ATTRIBUTES**

    *BOOL* active -- is grammar active?

    *[STR]* spell_words -- words which must precede the first spelled letter, 
    or None for an unrestricted spelling grammar.  

    *FCT(STR)* spelling_cbk -- callback to signal recognition
    Currently, the letters spelled are returned as a single string
    """
    def __init__(self, spell_words = None, spelling_cbk = None, **attrs):
        self.deep_construct(MilitarySpelling,
            {'active' : 0,
             'spell_words': spell_words,
             'spelling_cbk': spelling_cbk
            }, attrs)
        if spell_words:
            self.spell_words = map(self.capitalize_rule, spell_words)

    def activate(self, window):
        """activates the grammar for recognition tied to a window
        with the given handle

        **INPUTS**

        *INT* window -- window handle (unique identifier) for the window

        **OUTPUTS**

        *none*
        """
        debug.virtual('MilitarySpelling.activate')

    def cleanup(self):
        """method which must be called by the owner prior to deleting
        the grammar, to ensure that it doesn't have circular references
        to the owner
        """
        self.spelling_cbk = None

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('MilitarySpelling.deactivate')

        
class SimpleSelection(WinGram):
    """abstract base class for simple selection grammars

    **INSTANCE ATTRIBUTES**

    *[STR]* select_phrases -- words (or phrases) which introduces a
    selection utterance

    *STR* through_word -- word which separates the beginning and end of
    a range to be selected.

    *STR FCT() get_visible_cbk* -- callback for retrieving the visible range

    *(INT, INT) FCT() get_selection_cbk* -- callback for retrieving the 
    current selection

    *FCT(range) select_cbk* -- callback which returns the *(INT, INT)* 
    range to be selected (relative to the start of the visible range 
    passed to the get_visible_cbk), or None if text wasn't found

    *STR* visible -- currently visible range 

    *(INT, INT)* selection -- current selection 

    *INT* window -- window handle (unique identifier) for the window to
    which the grammar is specific
    """
    def __init__(self, select_phrases = ['select'],
        through_word = 'through', get_visible_cbk = None,
        get_selection_cbk = None, select_cbk = None, **attrs):
        self.deep_construct(SimpleSelection,
            {'select_phrases' : select_phrases, 'through_word' : through_word,
            'get_visible_cbk': get_visible_cbk,
            'get_selection_cbk': get_selection_cbk,
            'select_cbk': select_cbk,
            'visible': None,
            'selection': None,
            'window': None}, 
            attrs, enforce_value = {'manager': None})

    def cleanup(self):
        """clean up the grammar and any circular references
        """
        self.select_cbk = None
        self.get_selection_cbk = None
        self.get_visible_cbk = None

    def _set_visible(self, visible):
        """internal call to set the currently visible range.

        **INPUTS**

        *STR* visible -- visible text range 

        **OUTPUTS**

        *none*
        """
        debug.virtual('SimpleSelection._set_visible')

    def recognition_starting(self):
        """method which a concrete subclass should call when recognition
        is starting

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('SimpleSelection.recognition_starting', 'self.is_active()=%s, self.get_visible_cbk=%s' % (self.is_active(), self.get_visible_cbk))
        if self.is_active():
            self.visible = self.get_visible_cbk()
            self.selection = self.get_selection_cbk()
            self._set_visible(self.visible)
            if debug.tracing('SimpleSelection.recognition_starting'):
                debug.trace('SimpleSelection.recognition_starting', 
                    'visible = "%s"' % self.visible)
                debug.trace('SimpleSelection.recognition_starting', 
                    'selection = %d, %d' % self.selection)

    def activate(self, window):
        """activates the grammar for recognition tied to the current window,
        and checks with buffer for the currently visible range.

        **INPUTS**

        *INT* window -- window handle (unique identifier) for the window

        **OUTPUTS**

        *none*
        """
        debug.virtual('SimpleSelection.activate')

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('SimpleSelection.deactivate')

    def find_closest(self, verb, ranges):
        """Sort the ranges from earliest to latest, and select the one
        which is closest to the cursor in the proper direction

        **INPUTS**

        *STR* verb -- verb used by the selection

        *[(INT, INT)] -- list of ranges of offsets into visible range
        with the best recognition score
        """
        
        debug.trace('SimpleSelection.find_closest', 'verb=%s, ranges=%s' % 
                                                   (verb, repr(ranges)))
        
        #
        # Analyse the verb used by the user in the Select utterance
        #
        direction = None
        if re.search('previous', verb, re.IGNORECASE):
            direction = -1
        if re.search('next', verb, re.IGNORECASE):                
            direction = 1

        mark_selection = 1
        if re.search('go', verb, re.IGNORECASE) or \
                re.search('before', verb, re.IGNORECASE) or \
                re.search('after', verb, 1):
            mark_selection = 0

        where = 1
        if re.search('before', verb, re.IGNORECASE):
            where = -1
        if re.search('after', verb, re.IGNORECASE):
            where = 1

        selection = self.selection
        if selection[1] < selection[0]:
            selection = selection[1], selection[0]

        debug.trace('SimpleSelection.find_closest', 
                'direction = %s' % direction)
        debug.trace('SimpleSelection.find_closest', 
                'selection = %d, %d' % selection)

        ranges.sort()
        reversed = map(lambda x: (x[1], x[0]), ranges)
        reversed.sort()
        reversed.reverse()

        debug.trace('SimpleSelection.find_closest', 
                'ranges = %s' % repr(ranges))
        debug.trace('SimpleSelection.find_closest', 
                'reversed = %s' % repr(reversed))

        closest_before = None
        closest_before_end = None
        for i in range(len(reversed)):
            if reversed[i][0] <= selection[1]:
                closest_before_end = reversed[i]
            if reversed[i][0] <= selection[0]:
                closest_before = reversed[i]
                break
        if closest_before:
            closest_before = closest_before[1], closest_before[0]
        if closest_before_end:
            closest_before_end = closest_before_end[1], closest_before_end[0]

        debug.trace('SimpleSelection.find_closest', 
                'closest_before = %s' % repr(closest_before))
        debug.trace('SimpleSelection.find_closest', 
                'closest_before_end = %s' % repr(closest_before_end))

        closest_after = None
        closest_after_start = None
        for i in range(len(ranges)):
            if ranges[i][0] >= selection[0]:
                closest_after_start = ranges[i]
            if ranges[i][0] >= selection[1]:
                closest_after = ranges[i]
                break

        debug.trace('SimpleSelection.find_closest', 
                'closest_after = %s' % repr(closest_after))
        debug.trace('SimpleSelection.find_closest', 
                'closest_after_start = %s' % repr(closest_after_start))

        if direction is None:
            d_after = None
            if closest_after_start:
                if closest_after_start == closest_after:
                    d_after = closest_after_start[0] - selection[1] 
                else:
                    d_after = 0
            debug.trace('SimpleSelection.find_closest', 
                'd_after = %s' % d_after)
            d_before = None
            if closest_before_end:
                if closest_before_end == closest_before:
                    d_before = selection[0] - closest_before_end[1]
                else:
                    d_before = 0
            debug.trace('SimpleSelection.find_closest', 
                'd_before = %s' % d_before)
            if d_after is None:
                if d_before is None:
                    debug.trace('SimpleSelection.find_closest', 
                        'None at all')
                    closest = None
                else:
                    closest = closest_before_end 
                    debug.trace('SimpleSelection.find_closest', 
                        'only before: closest = %s' % repr(closest))
            elif d_before is None:
                closest = closest_after_start
                debug.trace('SimpleSelection.find_closest', 
                    'only after: closest = %s' % repr(closest))
            else:
                debug.trace('SimpleSelection.find_closest', 
                    'both: d_before, d_after = %d, %d' % (d_before, d_after))
                if d_after > d_before:
                    closest = closest_before_end 
                else:
                    closest = closest_after_start
                debug.trace('SimpleSelection.find_closest', 
                    'closest = %s' % repr(closest))
        elif direction < 0:
            closest = closest_before
        elif direction > 0:
            closest = closest_after

        if not closest:
            self.select_cbk(None) 
            return

        if mark_selection:
            self.select_cbk(closest)
        else:
            if where > 0:
                self.select_cbk((closest[1], closest[1]))
            else:
                self.select_cbk((closest[0], closest[0]))


class TextModeTogglingGram(WinGram):
    """Abstract base class for enabling-disabling text-mode.

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **attrs):
        self.deep_construct(TextModeTogglingGram,
            {}, attrs)


    def gram_type(self):
        """returns a subclass-dependent string describing the type of grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- type of grammar ('dictation', 'selection', 
        'correction', or 'text_mode')
        """
        return 'text_mode'
    
