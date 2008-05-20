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

"""implementation of sr_grammars classes using natlink
"""

from Object import Object
import sr_interface
from sr_grammars import *
import natlink
import debug
import exceptions
from natlinkutils import *
import string
import sys

import CmdInterp, AppState

def compose_alternatives(words):
    first = words[0]
    alternatives = first
    for word in words[1:]:
        alternatives = alternatives + ' | ' + word
    return "( %s )" % alternatives

class DictThroughCmdWinGramNL(DictWinGram, GrammarBase):
    """natlink implementation of DictWinGram for window-specific 
    dictation grammar interfaces

    **INSTANCE ATTRIBUTES**

    *CLASS* wave_playback -- class constructor for a concrete
    subclass of WavePlayback, or None if no playback is available

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, wave_playback = None, **attrs):
        """
        **INPUTS**

        *CLASS* wave_playback -- class constructor for a concrete
        subclass of WavePlayback, or None if no playback is available
        """


        self.deep_construct(DictThroughCmdWinGramNL,
            {'wave_playback': wave_playback}, attrs, 
            exclude_bases = {GrammarBase:1})
        GrammarBase.__init__(self)
        
        
        
        # AD: When these are set to 1, we generate grammars for dictating
        #     CSCs, LSAs and known symbol through a command grammar.
        #     This is meant to (not proven yet) improve the accuracy,
        #     but also, in the case of CSCs and LSAs, it prevents conflicts
        #     between a discrete NatSpeak command and a CSC or LSA with same
        #     spoken form (for example, the VCode CSC "copy that" would other
        #     not be recognised because the NatSpeak command "copy that" intercepts
        #     the utterance).
        #
        #     Note that in the case of generate_sym_gram,
        #     setting it to 1 creates very large grammars and NatSpeak usually
        #     chokes on them. I leave it here in case we eventually find a
        #     workaround this problem.
        #
        #     Also, currently in the case of generate_csc_lsa_gram, even if it set to 1,
        #     we only generate discrete commands for CSCs (not LSAs), and even there, we only
        #     do it for CSCs that have generate_discrete_cmd = 1 (i.e. commands where 
        #     we explicitly asked for a discrete command because we know the CSC conflicts
        #     with a NatSpeak command).
        #
        self.generate_csc_lsa_gram = 1
        self.generate_sym_gram = 0
        
        self.load(self._gram_spec())
        
    def _gram_spec(self):
        if not (self.generate_csc_lsa_gram or self.generate_sym_gram):
           # Need to provide a dummy grammar.
           gram_spec = "<vcode_utterance> exported = fdfasdfsdfasdfdgsgsdfgdfg;"
        else:
           gram_spec = """<dgndictation> imported;
                          <vcode_utterance> exported = <known_spoken_form>+ <dgndictation> | 
                                                       <dgndictation> <known_spoken_form>+;
                          <known_spoken_form> = <known_spoken_cmd>|<known_spoken_symbol>;          
                          """
           gram_spec = gram_spec + \
                       self.interpreter().gram_spec_spoken_cmd("known_spoken_cmd", not self.generate_csc_lsa_gram)
           gram_spec = gram_spec + \
                       self.interpreter().gram_spec_spoken_symbol("known_spoken_symbol", not self.generate_sym_gram)   
        
        if debug.tracing('DictThroughCmdWinGramNL._gram_spec'):
            debug.trace('DictThroughCmdWinGramNL._gram_spec', 'self=%s, returning "%s"' % (self, gram_spec))
                       
        return gram_spec        
                
    def gotBegin(self, moduleInfo):
        debug.trace('DictThroughCmdWinGramNL.gotBegin', 'invoked')            
        

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
        GrammarBase.setExclusive(self, exclusive)
        
    def activate(self):
        """activates the grammar for recognition
        tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('DictThroughCmdWinGramNL.activate', 
            '%s received activate'% self.buff_name)
        if not self.is_active():
            debug.trace('DictThroughCmdWinGramNL.activate', 'not already active')
            window = self.window
            if window == None:
                window = 0
            debug.trace('DictThroughCmdWinGramNL.activate', 
                'activating, window = %d, exclusive = %d' % (window,
                self.exclusive))
            GrammarBase.activateAll(self, window = window, 
                exclusive = self.exclusive)                
            self.active = 1

    def remove_other_references(self):
        GrammarBase.unload(self)
        DictWinGram.remove_other_references(self)

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('DictThroughCmdWinGramNL.deactivate', 
            '%s received deactivate' % self.buff_name)
        if self.is_active():
            debug.trace('DictThroughCmdWinGramNL.deactivate', 'was active')
            GrammarBase.deactivate(self, "vcode_utterance")
            self.active = 0

    def cleanup(self):
        self.deactivate()
        GrammarBase.unload(self)
        

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
        # AD: Note this method doesn't make sense for this class. 
        #     Remove it later.
        pass

    def gotResults_vcode_utterance(self, words, fullResults):
       print '-- DictThroughCmdWinGramNL.gotResults_vcode_utterance: words=%s' % repr(words)
       
    def gotResults_known_spoken_form(self, words, fullResults):
       print '-- DictThroughCmdWinGramNL.gotResults_known_spoken_form: words=%s' % repr(words)
    
    def gotResults_known_spoken_form(self, words, fullResults):
       print '-- DictThroughCmdWinGramNL.gotResults_known_spoken_form: words=%s' % repr(words)
    
    def gotResults_known_spoken_symbol(self, words, fullResults):        
       print '-- DictThroughCmdWinGramNL.gotResults_known_spoken_symbol: words=%s' % repr(words)

    def gotResultsObject(self, recogType, results):
            debug.trace('DictThroughCmdWinGramNL.gotResultsObject', 
                'recogType=%s, results=%s, self.exclusive=%s, self.buff_name=%s' % \
                (recogType, repr(results), self.exclusive, self.buff_name))
            if recogType == 'self':
                utterance = \
                    sr_interface.SpokenUtteranceNL(results, self.wave_playback)
                self.on_results(utterance)
#                self.last = SpokenUtteranceNL(results)
# not sure if yet if this is where we should store the utterance
#                words = results.getWords(0)
#                interp = self.interpreter()
#                interp.interpret_NL_cmd(words, self.app,
#                    initial_buffer = self.buff_name)
#                self.app.print_buff_if_necessary(buff_name
#                    = self.buff_name)
            else:
                 if debug.tracing('DictThroughCmdWinGramNL.gotResultsObject'):
                     debug.trace('DictThroughCmdWinGramNL.gotResultsObject',
                         'results=%s' % repr(results))


class DictWinGramNL(DictWinGram, DictGramBase):
    """natlink implementation of DictWinGram for window-specific 
    dictation grammar interfaces

    **INSTANCE ATTRIBUTES**

    *CLASS* wave_playback -- class constructor for a concrete
    subclass of WavePlayback, or None if no playback is available

    **CLASS ATTRIBUTES**

    *none*
    """
        
    
    def __init__(self, wave_playback = None, **attrs):
        """
        **INPUTS**

        *CLASS* wave_playback -- class constructor for a concrete
        subclass of WavePlayback, or None if no playback is available
        """
        self.deep_construct(DictWinGramNL,
            {'wave_playback': wave_playback}, attrs, 
            exclude_bases = {DictGramBase:1})
        DictGramBase.__init__(self)
#        self.load(allResults=1)
        self.load()

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
        DictGramBase.setExclusive(self, exclusive)
        
    def activate(self):
        """activates the grammar for recognition
        tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('DictWinGramNL.activate', 
            '%s received activate'% self.buff_name)
        if not self.is_active():
            debug.trace('DictWinGramNL.activate', 'not already active')
            window = self.window
            if window == None:
                window = 0
            debug.trace('DictWinGramNL.activate', 
                'activating, window = %d, exclusive = %d' % (window,
                self.exclusive))
            DictGramBase.activate(self, window = window, 
                exclusive = self.exclusive)
            self.active = 1

    def remove_other_references(self):
        DictGramBase.unload(self)
        DictWinGram.remove_other_references(self)

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('DictWinGramNL.deactivate', 
            '%s received deactivate' % self.buff_name)
        if self.is_active():
            debug.trace('DictWinGramNL.deactivate', 'was active')
            DictGramBase.deactivate(self)
            self.active = 0

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
        self.setContext(before, after)

    def gotResultsObject(self, recogType, results):
            debug.trace('DictWinGramNL.gotResultsObject', 
                'recogType=%s, results=%s, self.exclusive=%s, self.buff_name=%s' % \
                (recogType, repr(results), self.exclusive, self.buff_name))
                
            debug.trace('DictWinGramNL.gotResultsObject', '** upon entry, self.app.curr_buffer().buff_name=%s' % self.app.curr_buffer().buff_name)
            if recogType == 'self':
                utterance = \
                    sr_interface.SpokenUtteranceNL(results, self.wave_playback)
                self.on_results(utterance)
#                self.last = SpokenUtteranceNL(results)
# not sure if yet if this is where we should store the utterance
#                words = results.getWords(0)
#                interp = self.interpreter()
#                interp.interpret_NL_cmd(words, self.app,
#                    initial_buffer = self.buff_name)
#                self.app.print_buff_if_necessary(buff_name
#                    = self.buff_name)
            else:
                 if debug.tracing('DictWinGramNL.gotResultsObject'):
                     debug.trace('DictWinGramNL.gotResultsObject', 
                         'results=%s' % repr(results))
             


class SelectWinGramNL(SelectWinGram, SelectGramBase):
    """natlink implementation of SelectWinGram for window-specific 
    selection grammar interfaces

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **attrs):
        self.deep_construct(SelectWinGramNL,
            {}, attrs, exclude_bases = {SelectGramBase:1})
        SelectGramBase.__init__(self)
        self.load(selectWords = self.select_phrases, throughWord =
            self.through_word)
            
    def gotBegin(self, moduleInfo):
        debug.trace('SelectWinGramNL.gotBegin', 'invoked')            

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
        SelectGramBase.setExclusive(self, exclusive)
        
    def _set_visible(self, visible):
        """internal call to set the currently visible range.

        **INPUTS**

        *STR* visible -- visible text range 

        **OUTPUTS**

        *none*
        """
        if debug.tracing('SelectWinGramNL._set_visible'):
            debug.trace('SelectWinGramNL._set_visible', 'visible=%s' % visible)
        SelectGramBase.setSelectText(self, visible)

    def activate(self, buff_name):
        """activates the grammar for recognition tied to the current window,
        and checks with buffer for the currently visible range.

        **INPUTS**

        *STR* buff_name -- name of currently active buffer

        **OUTPUTS**

        *none*
        """
        debug.trace('SelectGramBase.activate', 'invoked, buff_name=%s' % buff_name)
        self.buff_name = buff_name
        self.find_visible()

        if not self.is_active():
            window = self.window
            if window == None:
                window = 0
            SelectGramBase.activate(self, window = window, 
                exclusive = self.exclusive)
            self.active = 1


    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('SelectWinGramNL.deactivate', 'self.buff_name=%s' % self.buff_name)
        if self.is_active():
            SelectGramBase.deactivate(self)
            self.active = 0

    def cleanup(self):
        SelectGramBase.unload(self)
        SelectWinGram.cleanup(self)
    
    def gotResultsObject(self,recogType,resObj):
        debug.trace('SelectWinGramNL.gotResultsObject', '** invoked, resObj=%s' % repr(resObj))
        if recogType == 'self':
            utterance = sr_interface.SpokenUtteranceNL(resObj)
            self.results_callback(utterance)
            debug.trace('SelectWinGramNL.gotResultsObject', '** recogType = self')        
            # If there are multiple matches in the text we need to scan through
            # the list of choices to find every entry which has the highest.
            
            ranges = []        
            try:
                bestScore = resObj.getWordInfo(0)[0][2]
                verb = resObj.getWordInfo(0)[0][0]
                #
                # Collect selection ranges with highest score
                #
                for i in range(100):
                    #
                    # The candidate regions are sorted from best to worst scores.
                    # Loop through candidate regions until we reach one whose
                    # score is not the same as the first score (or until a
                    # natlink.outOfRange exception is raised to signal the end
                    # of the list of candidate regions).
                    #
                    wordInfo = resObj.getWordInfo(i)
#                    debug.trace('SelectWinGramNL.gotResultsObject', '** i=%s, len(wordInfo)=%s' % (i, len(wordInfo)))
#                    debug.trace('SelectWinGramNL.gotResultsObject', '** i=%s, len(wordInfo[0])=%s' % (i, len(wordInfo[0])))                    
                    if wordInfo[0][2] != bestScore:
                        #
                        # All remaining regions don't have as good a score as the
                        # first ones.
                        break
                    else:
                        #
                        # This region has the same score as the first ones. Add it
                        # to the candidate selection ranges.
                        #
                        #QH: if a results object is not of this Select Grammar
                        #also break
                        try:
                            region = resObj.getSelectInfo(self.gramObj, i)
                        except natlink.WrongType:
                            continue
                        if debug.tracing('SelectWinGramNL.gotResultsObject'):
                            debug.trace('SelectWinGramNL.gotResultsObject', 'adding region=%s' % repr(region))
                        true_region = (region[0] + self.vis_start,
                          region[1] + self.vis_start)
                        #
                        # For some reason, NatSpeak may return duplicate ranges
                        #
                        if not true_region in ranges:
                           ranges.append(true_region)

            except natlink.OutOfRange, exceptions.IndexError:
                pass

            spoken = self.selection_spoken_form(resObj)
            debug.trace('SelectWinGramNL.gotResultsObject', 'verb=%s, spoken=%s, ranges=%s' % (verb, spoken, repr(ranges)))
            self.find_closest(verb, spoken, ranges)

    def selection_spoken_form(self, resObj):

        """Returns the spoken form of the selected part of a *Select
        Pseudocode* utterance.
        
        **INPUTS**
        
        *ResObj* resObj -- The *ResObj* returned by the *Select* grammar.
        

        **OUTPUTS**
        
        *spoken_form* -- The spoken form of the selected code.
        """
                
        spoken_form = ''
        #
        # Ignore first word because it is the verb
        #
        for a_word_info in resObj.getWordInfo(0)[1:]:
            a_spoken_word, dummy = sr_interface.spoken_written_form(a_word_info[0])
            if spoken_form != '':
                spoken_form = spoken_form + ' '
            spoken_form = spoken_form + a_spoken_word

#        print '-- CodeSelectGrammar.selection_spoken_form: returning spoken_form=%s' % spoken_form
        
        return spoken_form

class BasicCorrectionWinGramNL(BasicCorrectionWinGram, GrammarBase):
    """natlink implementation of BasicCorrectionWinGram for window-specific 
    basic correction grammars

    **INSTANCE ATTRIBUTES**

    *{STR: [STR]} lists* -- map from list names to the initial values
    to be assigned to them once the grammar is loaded

    *[STR] rules* -- list of rules for this grammar

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **attrs):
        self.deep_construct(BasicCorrectionWinGramNL,
            {'lists': {},
             'rules': []}, attrs, exclude_bases = {GrammarBase:1})
        GrammarBase.__init__(self)
        self.create_rules()
        self.load()

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
        GrammarBase.setExclusive(self, exclusive)
        
    def load(self):
        if self.rules:
            GrammarBase.load(self, self.rules)
            for name, values in self.lists.items():
                self.setList(name, values)

    def create_rules(self):
        """create all the rules for this grammar and put them in
        self.rules

        **INPUTS** 

        *none*

        **OUTPUTS**

        *none*
        """
        self.rules = []
        self.rules.extend(self.scratch_rule())
        self.rules.extend(self.correct_rule())
        self.rules.extend(self.correct_recent_rule())

    def scratch_rule(self):
        """create the rules for the grammar recognizing Scratch That/n

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of strings comprising the rules needed to
        implement the Scratch That and Scratch n commands
        """
        if not self.scratch_words:
            return []
        scratch = compose_alternatives(self.scratch_words)
        rules = []
        that = self.capitalize_rule('that')
        last = self.capitalize_rule('last')
        rules.append("<scratch_that> exported = %s %s;" % (scratch, that))
        rules.append("<scratch_n> exported = %s %s {count};" % \
            (scratch, last))
        self.lists['count'] = ['1', '2', '3', '4', '5']
        return rules

    def correct_rule(self):
        """create the rules for the grammar recognizing Scratch That/n

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of strings comprising the rules needed to
        implement the Correct That commands
        """
        if not self.correct_words:
            return []
        correct = compose_alternatives(self.correct_words)
        rules = []
        that = self.capitalize_rule('that')
        rules.append("<correct_that> exported = %s %s;" % (correct, that))
        return rules

    def correct_recent_rule(self):
        """create the rules for the grammar recognizing Correct Recent

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of strings comprising the rules needed to
        implement the Correct Recent commands
        """
        if not self.correct_words:
            return []
        if not self.recent_words:
            return []
        correct = compose_alternatives(self.correct_words)
        recent = compose_alternatives(self.recent_words)
        rules = []
        rules.append("<correct_recent> exported = %s %s;" \
            % (correct, recent))
        return rules

    def activate(self):
        """activates the grammar for recognition
        tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('BasicCorrectionWinGramNL.activate', 
            'received activate')
        if not self.is_active():
            debug.trace('BasicCorrectionWinGramNL.activate', 
                'not already active')
            window = self.window
            if window == None:
                window = 0
            GrammarBase.activateAll(self, window = window, 
                exclusive = self.exclusive)
            self.active = 1

    def remove_other_references(self):
        GrammarBase.unload(self)
        BasicCorrectionWinGram.remove_other_references(self)

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('BasicCorrectionWinGramNL.activate', 
            'received deactivate')
        if self.is_active():
            debug.trace('BasicCorrectionWinGramNL.activate', 
                'was active')
            GrammarBase.deactivateAll(self)
            self.active = 0

    def gotResults_scratch_that(self, words, fullResults):
        """handler for scratch that command
        """
        debug.trace('BasicCorrectionWinGramNL.gotResults_scratch_that',
            'heard scratch that')
        self.scratch_recent(1)

    def gotResults_scratch_n(self, words, fullResults):
        """handler for scratch that command
        """
        count = int(words[-1])
        debug.trace('BasicCorrectionWinGramNL.gotResults_scratch_that',
            'heard scratch n, n = %d' % count)
        self.scratch_recent(count)

    def gotResults_correct_that(self, words, fullResults):
        """handler for scratch that command
        """
        debug.trace('BasicCorrectionWinGramNL.gotResults_scratch_that',
            'heard correct that')
        self.on_correct_last()

    def gotResults_correct_recent(self, words, fullResults):
        """handler for scratch that command
        """
        debug.trace('BasicCorrectionWinGramNL.gotResults_scratch_that',
            'heard correct recent')
        self.on_correct_recent()

    def gotResultsObject(self, recog_type, results):
        debug.trace('BasicCorrectionWinGramNL.gotResultsObject', 'invoked')
        if recog_type == 'self':
            utterance = sr_interface.SpokenUtteranceNL(results)
            self.results_callback(utterance)

class SymbolReformattingWinGramNL(SymbolReformattingWinGram, GrammarBase):
    """natlink implementation of SymbolReformattingWinGram for window-specific 
    symbol correction grammars

    **INSTANCE ATTRIBUTES**

    *[STR] rules* -- list of rules for this grammar

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **attrs):
        self.deep_construct(SymbolReformattingWinGramNL,
            {'rules': []}, attrs, exclude_bases = {GrammarBase:1})
        GrammarBase.__init__(self)
        self.create_rules()
        self.load()

    def gotResults_reformat_recent(self, words, fullResults):
        """handler for Reformat Recent command
        """
        debug.trace('BasicCorrectionWinGramNL.gotResults_reformat_recent',
            'heard reformat recent')
        self.on_reformat_recent()
        
    def on_reformat_recent(self):
        self.manager.reformat_recent()

    def load(self):
        if self.rules:
            GrammarBase.load(self, self.rules)

    def create_rules(self):
        """create all the rules for this grammar and put them in
        self.rules

        **INPUTS** 

        *none*

        **OUTPUTS**

        *none*
        """
        self.rules = []
        self.rules.extend(self.reformat_recent_rule())

    def reformat_recent_rule(self):
        """create the rules for the grammar recognizing Reformat Recent/n

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of strings comprising the rules needed to
        implement the Reformat Recent commands
        """
        if not self.reformat_words:
            return []
        if not self.recent_words:
            return []
        reformat = compose_alternatives(self.reformat_words)
        recent = compose_alternatives(self.recent_words)
        rules = []
        rules.append("<reformat_recent> exported = %s %s;" \
            % (reformat, recent))
                    
        return rules

    def activate(self):
        """activates the grammar for recognition
        tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('SymbolReformattingWinGramNL.activate', 
            'received activate')
        if not self.is_active():
            debug.trace('SymbolReformattingWinGramNL.activate', 
                'not already active')
            window = self.window
            if window == None:
                window = 0
            GrammarBase.activateAll(self, window = window, 
                exclusive = self.exclusive)
            self.active = 1

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('SymbolReformattingWinGramNL.activate', 
            'received deactivate')
        if self.is_active():
            debug.trace('SymbolReformattingWinGramNL.activate', 
                'was active')
            GrammarBase.deactivateAll(self)
            self.active = 0

    def remove_other_references(self):
        GrammarBase.unload(self)
        SymbolReformattingWinGram.remove_other_references(self)
        
    def _set_exclusive_when_active(self, exclusive):
        """private method which ensures that even currently active grammars 
        become exclusive.  This is important because activate may be 
        ignored if the grammar is already active, so a change to
        self.exclusive may not take effect even on the next utterance

        **INPUTS**

        *BOOL* exclusive -- true if the grammar should be exclusive

        **OUTPUTS**

        *none*
        """
        GrammarBase.setExclusive(self, exclusive)

class DiscreteCSCsAndLSAsNL(DiscreteCSCsAndLSAsWinGram, GrammarBase):
    """This grammar recognises CSCs and LSAs whose spoken form happens to
    be a NatSpeak discrete command (ex: "copy that"). Whithout this grammar, 
    the NatSpeak discrete command would take precedence over the VCode 
    LSA or CSA.

    **INSTANCE ATTRIBUTES**
    
    *[STR] LSAs* -- spoken forms of the discrete LSAs
     
    *[STR] CSCs* -- spoken forms of the discrete CSCs

    *[STR] rules* -- list of rules for this grammar

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **attrs):
        self.deep_construct(DiscreteCSCsAndLSAsNL,
            {'rules': [], LSAs: [], CSCs: []}, attrs, 
            exclude_bases = {GrammarBase:1})
        GrammarBase.__init__(self)
        self.create_rules()
        self.load()

    def gotResults_discrete_LSA(self, words, fullResults):
        """handler for a discrete LSA
        """
        pass
#        self.manager.interpret_something(????)

    def gotResults_discrete_CSC(self, words, fullResults):
        """handler for a discrete LSA
        """
        pass
#        self.manager.interpret_something(????)
        
    def load(self):
        if self.rules:
            GrammarBase.load(self, self.rules)

    def create_rules(self):
        """create all the rules for this grammar and put them in
        self.rules

        **INPUTS** 

        *none*

        **OUTPUTS**

        *none*
        """
        self.rules = []
        LSA_rule = None
        if len(self.LSAs) > 0:
            LSA_rule = "<discrete_LSA> exported = "
            first = 1
            for a_LSA in self.LSAs:
               if not first:
                  first = None
                  LSA_rule = "%s|" % LSA_rule
               LSA_rule = "%s%s" % (LSA_rule, a_LSA)
            LSA_rule = "%s;" % LSA_rule
                  
        CSC_rule = None
        if len(self.CSCs) > 0:
            CSC_rule = "<discrete_CSC> exported = "
            first = 1
            for a_CSC in self.CSCs:
               if not first:
                  first = None
                  CSC_rule = "%s|" % CSC_rule
               CSC_rule = "%s%s" % (CSC_rule, a_CSC)
            CSC_rule = "%s;" % CSC_rule
            
        return [LSA_rule, CSC_rule]

    def activate(self):
        """activates the grammar for recognition
        tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('DiscreteCSCsAndLSAsNL.activate', 
            'received activate')
        if not self.is_active():
            debug.trace('DiscreteCSCsAndLSAsNL.activate', 
                'not already active')
            window = self.window
            if window == None:
                window = 0
            GrammarBase.activateAll(self, window = window, 
                exclusive = self.exclusive)
            self.active = 1

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('DiscreteCSCsAndLSAsNL.activate', 
            'received deactivate')
        if self.is_active():
            debug.trace('DiscreteCSCsAndLSAsNL.activate', 
                'was active')
            GrammarBase.deactivateAll(self)
            self.active = 0

    def remove_other_references(self):
        GrammarBase.unload(self)
        SymbolReformattingWinGram.remove_other_references(self)
        
    def _set_exclusive_when_active(self, exclusive):
        """private method which ensures that even currently active grammars 
        become exclusive.  This is important because activate may be 
        ignored if the grammar is already active, so a change to
        self.exclusive may not take effect even on the next utterance

        **INPUTS**

        *BOOL* exclusive -- true if the grammar should be exclusive

        **OUTPUTS**

        *none*
        """
        GrammarBase.setExclusive(self, exclusive)

    
class WinGramFactoryNL(WinGramFactory):
    """natlink implementation of factory which returns 
    window-specific grammars

    **INSTANCE ATTRIBUTES**

    *CLASS* wave_playback -- class constructor for a concrete
    subclass of WavePlayback, or None if no playback is available

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, wave_playback = None, **attrs):
        """
        **INPUTS**

        *CLASS* wave_playback -- class constructor for a concrete
        subclass of WavePlayback, or None if no playback is available

        **OUTPUTS**

        *none*
        """
        self.deep_construct(WinGramFactoryNL,
            {'wave_playback': wave_playback}, attrs)

    def make_dictation(self, manager, app, buff_name, window = None,
        exclusive = 0):
        """create a new dictation grammar

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

        *DictWinGramNL* -- new dictation grammar for supporting VoiceCode dictation.
        """
        return DictWinGramNL(manager = manager, app = app, 
            buff_name = buff_name, window = window, exclusive = exclusive,
            wave_playback = self.wave_playback) 
    
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
        debug.trace('WinGramFactoryNL.make_dictation_through_cmd', 'exclusive=%s' % exclusive)
        return DictThroughCmdWinGramNL(manager = manager, app = app, 
            buff_name = buff_name, window = window, exclusive = exclusive,
            wave_playback = self.wave_playback)    
    
    def make_selection(self, manager, app, window = None, buff_name = None,
        exclusive = 0):
        """create a new selection grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the
        grammar

        *AppState* app -- application corresponding to the selection
        grammar, which is queried with buff_name for the currently
        visible range, and is notified of selection changes

        *STR* buff_name -- name of the buffer corresponding to this
        grammar.  Can also be set later in the activate call to the
        grammar.

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *SelectWinGram* -- new selection grammar
        """
        return SelectWinGramNL(manager = manager, app = app, 
            buff_name = buff_name, select_phrases =
            self._select_phrases(), through_word = self.through_word, 
            window = window, exclusive = exclusive, 
            capitalize_rules = self.capitalize) 
    
    def make_correction(self, manager, window = None, exclusive = 0):
        """create a new basic correction grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the
        grammar

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *BasicCorrectionWinGram* -- new basic correction grammar
        """
        return BasicCorrectionWinGramNL(manager = manager, scratch_words =
            self.scratch_words, correct_words = self.correct_words, 
            recent_words = self.recent_words, 
            window = window, exclusive = exclusive, 
            capitalize_rules = self.capitalize) 

    def make_reformatting(self, manager, window = None, exclusive = 0):
        """create a new symbol reformatting grammar

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the
        grammar

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *BasicCorrectionWinGram* -- new basic correction grammar
        """
        return SymbolReformattingWinGramNL(manager = manager, 
            reformat_words = self.reformat_words, 
            recent_words = self.recent_words,
            window = window, exclusive = exclusive, 
            capitalize_rules = self.capitalize) 

    def make_discrete_csc_lsa(self, manager, window = None, exclusive = 0):
        """create a new grammar for recognising discrete CSCs and LSAs

        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the
        grammar

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *DiscreteCSCsAndLSAsNL* -- new basic correction grammar
        """
        return DiscreteCSCsAndLSAsNL(manager = manager, 
            window = window, exclusive = exclusive, 
            capitalize_rules = self.capitalize) 


    def make_text_mode(self, manager, on_spoken_as, off_spoken_as,
            off_sets_nat_text_to, window = None, exclusive = 0):
        """Create a new grammar for toggling text-mode on and off.
        
        **INPUTS**

        *WinGramMgr* manager -- the grammar manager which will own the
        grammar
        
        *[STR] on_spoken_as* -- list of spoken forms for the command
        that turns text mode on.
        
        *[STR] off_spoken_as* -- list of spoken forms for the command
        that turns text mode off.
             
        *0-1 off_sets_nat_text_to* -- when setting text mode off, put
        nat text into that state.

        *INT* window -- make grammar specific to a particular window

        *BOOL* exclusive -- is grammar exclusive?  (prevents other
        non-exclusive grammars from getting results)

        **OUTPUTS**

        *TextModeGram* -- the command grammar for toggling text-mode.
        
        """
        return TextModeTogglingGramNL(on_spoken_as, off_spoken_as,
            off_sets_nat_text_to, manager = manager, window = window, 
            exclusive=exclusive,
            capitalize_rules = self.capitalize) 
            
            
    def make_choices(self, choice_words):
        """create a new ChoiceGram choice grammar

        **INPUTS**

        *[STR]* choice_words -- grammar will be <choice_words> 1toN

        **OUTPUTS**

        *ChoiceGram* -- new choice grammar
        """
        return ChoiceGramNL(choice_words = choice_words,
            capitalize_rules = self.capitalize) 

    def make_natural_spelling(self, spell_words = None, spelling_cbk = None):
        """create a new NaturalSpelling grammar

        **INPUTS**

        *[STR]* spell_words -- words which must proceed the first spelled 
        letter, or None for an unrestricted spelling grammar.  The latter is not
        advisable unless dictation is disabled.

        *FCT(STR)* spelling_cbk -- callback to signal recognition.
        Currently, the letters or numbers spelled are returned as a single 
        string (with double-o, etc. expanded)

        **OUTPUTS**

        *NaturalSpelling* -- the spelling grammar
        """
        return NaturalSpellingNL(spell_words = spell_words, spelling_cbk =
            spelling_cbk, capitalize_rules = self.capitalize) 
   
    def make_military_spelling(self, spell_words = None, spelling_cbk = None):
        """create a new MilitarySpelling grammar

        **INPUTS**

        *[STR]* spell_words -- words which must proceed the first spelled 
        letter, or None for an unrestricted spelling grammar.  The latter is not
        advisable unless dictation is disabled.

        *FCT(STR)* spelling_cbk -- callback to signal recognition.
        Currently, the letters or numbers spelled are returned as a single 
        string (with double-o, etc. expanded)

        **OUTPUTS**

        *MilitarySpelling* -- the spelling grammar
        """
        return MilitarySpellingNL(spell_words = spell_words, spelling_cbk =
            spelling_cbk, capitalize_rules = self.capitalize) 
   
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
        if alt_select_phrases is None:
            select_phrases = self._select_phrases()
        else:
            select_phrases = alt_select_phrases
        return SimpleSelectionNL(select_phrases = select_phrases,
            get_visible_cbk = get_visible_cbk,
            get_selection_cbk = get_selection_cbk, 
            select_cbk = select_cbk,
            capitalize_rules = self.capitalize) 
            

class ChoiceGramNL(ChoiceGram, GrammarBase):
    """natlink implementation of ChoiceGram

    **INSTANCE ATTRIBUTES**

    *FCT(INT)* choice_cbk -- callback to signal recognition

    *[STR] rules* -- list of rules for this grammar in natlink's format

    *{STR: [STR]} lists* -- map from list names to the initial values
    to be assigned to them once the grammar is loaded
    """
    def __init__(self, **attrs):
        self.deep_construct(ChoiceGramNL,
            {
             'choice_cbk': None,
             'rules': [],
             'lists': {}
            }, attrs, 
            exclude_bases = {GrammarBase:1})
        GrammarBase.__init__(self)
        self.create_rules()
        self.load()

    def load(self):
        if self.rules:
            GrammarBase.load(self, self.rules)
            self.load_lists()

    def empty_lists(self):
        for name in self.lists.keys():
            self.emptyList(name)

    def load_lists(self):
        for name, values in self.lists.items():
            self.setList(name, values)

    def create_rules(self):
        """create all the rules for this grammar and put them in
        self.rules

        **INPUTS** 

        *none*

        **OUTPUTS**

        *none*
        """
        if not self.choice_words:
            self.rules = []
            return
        first = self.choice_words[0]
        choose = compose_alternatives(self.choice_words)
        rules = []
        rules.append("<choose_n> exported = %s {count};" % choose)
        self.lists['count'] = map(str, range(1, 10))
        self.rules = rules

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
        self.choice_cbk = choice_cbk
        self.empty_lists()
        self.lists['count'] = map(str, range(1, n + 1))
        self.load_lists()
        if not self.active:
            self.active = 1
            self.activateAll(window = window)
    
    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.choice_cbk = None
        if self.active:
            self.active = 0
            self.deactivateAll()
    
    def cleanup(self):
        """method which must be called by the owner prior to deleting
        the grammar, to ensure that it doesn't have circular references
        to the owner
        """
        self.deactivate()
        self.empty_lists()
        GrammarBase.unload(self)

    def gotResults_choose_n(self, words, fullResults):
        count = int(words[-1])
        self.choice_cbk(count)

class NaturalSpellingNL(NaturalSpelling, GrammarBase):
    """natlink implemenation of NaturalSpelling 

    **INSTANCE ATTRIBUTES**

    *[STR] rules* -- list of rules for this grammar in natlink's format
    """
    def __init__(self, **attrs):
        self.deep_construct(NaturalSpellingNL,
            {
             'rules': [],
            }, attrs, 
            exclude_bases = {GrammarBase:1})
        GrammarBase.__init__(self)
        self.create_rules()
        self.load()

    def load(self):
        if self.rules:
            GrammarBase.load(self, self.rules)

    def create_rules(self):
        """create all the rules for this grammar and put them in
        self.rules

        **INPUTS** 

        *none*

        **OUTPUTS**

        *none*
        """
        rules = ["<dgnletters> imported;"]
        spell = ""
        if self.spell_words:
            spell = compose_alternatives(self.spell_words) + " "
        rules.append("<natural_spelling> exported = %s<dgnletters>;" % spell) 
        self.rules = rules

    def activate(self, window):
        """activates the grammar for recognition tied to a window
        with the given handle

        **INPUTS**

        *INT* window -- window handle (unique identifier) for the window

        **OUTPUTS**

        *none*
        """
        if not self.active:
            self.active = 1
            self.activateAll(window = window)
    
    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.active:
            self.active = 0
            self.deactivateAll()
    
    def cleanup(self):
        """method which must be called by the owner prior to deleting
        the grammar, to ensure that it doesn't have circular references
        to the owner
        """
        self.deactivate()
        GrammarBase.unload(self)
        NaturalSpelling.cleanup(self)

    def gotResults_dgnletters(self, words, fullResults):
        s = ""
        cap_next = 0
        caps_on = 0
        for word in words:
#            print "word is [%s]" % word
            if re.match(r'\\', word):
#                print 'starts with slash'
                if re.match(r'\\space-bar$', word, re.IGNORECASE):
                    cap_next = 0
                    s = s + ' '
                    continue
                if re.match(r'\\Cap$', word, re.IGNORECASE):
                    cap_next = 1
                    continue
                elif re.match(r'\\Caps-On$', word, re.IGNORECASE):
                    caps_on = 1
                    continue
                elif re.match(r'\\Caps-Off$', word, re.IGNORECASE):
                    caps_on = 0
                    cap_next = 0
                    continue
                elif re.match(r'\\All-Caps-Off', word, re.IGNORECASE):
                    caps_on = 0
                    cap_next = 0
                    continue
                elif re.match(r'\\All-Caps', word, re.IGNORECASE):
                    caps_on = 1
                    continue
                continue
#            print "testing letter"
            letter_match = re.match(r'([A-Za-z])\\\\l$', word)
            if letter_match:
                c = letter_match.group(1)
#                print "matches letter %s" % c
                if cap_next or caps_on:
                    c = string.upper(c)
#                print "matches letter %s" % c
                s = s + c
                cap_next = 0
                continue
            explicit_match = re.match(r'([A-Za-z])\\letter-.*\\l$', word)
            if explicit_match:
                c = explicit_match.group(1)
#                print "matches letter %s" % c
                if cap_next or caps_on:
                    c = string.upper(c)
#                print "matches letter %s" % c
                s = s + c
                cap_next = 0
                continue
#            print "testing double"
            military_match = re.match(r'([A-Za-z])\\.*\\h$', word)
            if military_match:
                c = military_match.group(1)
#                print "matches letter %s" % c
                if cap_next or caps_on:
                    c = string.upper(c)
#                print "matches letter %s" % c
                s = s + c
                cap_next = 0
                continue
#            print "testing double"
            double_match = re.match(r'double-([A-Za-z])\\\\l$', word)
# I'm not sure if this and the alternate form below both occur, or only
# the alternate form -- for now, leave both to be sure
            if double_match:
                c = double_match.group(1) * 2
#                print "matches double %s" % c
                if cap_next or caps_on:
                    c = string.upper(c)
#                print "matches double %s" % c
                s = s + c
                cap_next = 0
                continue
#            print "testing digit"
            spoken, written = sr_interface.spoken_written_form(word)
            alt_double_match = re.match(r'(.*)\\double-', written)
            if alt_double_match:
                written = alt_double_match.group(1)
            if cap_next or caps_on:
                written = string.upper(written)
#            print "word: %s spoken: %s written: %s" % (word, spoken, written)
            s = s + written
            cap_next = 0

        self.spelling_cbk(s)

class MilitarySpellingNL(MilitarySpelling, GrammarBase):
    """implementation of MilitarySpelling using natlink

    **INSTANCE ATTRIBUTES**

    *{STR: [STR]} lists* -- map from list names to the initial values
    to be assigned to them once the grammar is loaded

    *[STR] rules* -- list of rules for this grammar

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, spell_words = None, spelling_cbk = None, **attrs):
        self.deep_construct(MilitarySpellingNL,
            {'lists': {},
             'rules': []}, attrs, exclude_bases = {GrammarBase:1})
        GrammarBase.__init__(self)
        self.create_rules()
        self.load()

    def load(self):
        if self.rules:
            GrammarBase.load(self, self.rules)
            for name, values in self.lists.items():
                self.setList(name, values)

    def create_rules(self):
        """create all the rules for this grammar and put them in
        self.rules

        **INPUTS** 

        *none*

        **OUTPUTS**

        *none*
        """
        self.rules = []
        self.letter_list()
        self.digit_list()
        self.other_lists()
        self.rules.extend(self.character_rule())
        self.rules.extend(self.spell_rule())

    def digit_list(self):
        self.lists['digits'] = ['0', '1', '2', '3', '4', '5', '6', '7',
            '8', '9']

    def other_lists(self):
        self.lists['formatting'] = \
          [
            r'\\space-bar',
            r'\\Cap',
            r'\\Caps-On',
            r'\\Caps-Off',
            r'\\All-Caps-Off',
            r'\\All-Caps',
            r'\\All-Caps-On'
          ]

    def character_rule(self):
        rules = ["<character> = ( {formatting} | {letters} | {prefixed_letters} | {digits} );"]
        return rules

    def spell_rule(self):
        spell = ""
        if self.spell_words:
            spell = compose_alternatives(self.spell_words) + " "
        rules = ["<mil_spell> exported = %s<character>+;" % spell]
        return rules

    def letter_list(self):
        self.lists['letters'] = \
          [ r'a\alpha',
            r'b\bravo',
            r'c\charlie',
            r'd\delta',
            r'e\echo',
            r'f\foxtrot',
            r'g\golf',
            r'h\hotel',
            r'i\india',
            r'j\juliett',
            r'k\kilo',
            r'l\lima',
            r'm\mike',
            r'n\november',
            r'o\oscar',
            r'p\papa',
            r'q\quebec',
            r'r\romeo',
            r's\sierra',
            r't\tango',
            r'u\uniform',
            r'v\victor',
            r'w\whiskey',
            r'x\xray',
            r'y\yankee',
            r'z\zulu' ]

        self.lists['prefixed_letters'] = \
          [ r'a\letter-alpha',
            r'b\letter-bravo',
            r'c\letter-charlie',
            r'd\letter-delta',
            r'e\letter-echo',
            r'f\letter-foxtrot',
            r'g\letter-golf',
            r'h\letter-hotel',
            r'i\letter-india',
            r'j\letter-juliett',
            r'k\letter-kilo',
            r'l\letter-lima',
            r'm\letter-mike',
            r'n\letter-november',
            r'o\letter-oscar',
            r'p\letter-papa',
            r'q\letter-quebec',
            r'r\letter-romeo',
            r's\letter-sierra',
            r't\letter-tango',
            r'u\letter-uniform',
            r'v\letter-victor',
            r'w\letter-whiskey',
            r'x\letter-xray',
            r'y\letter-yankee',
            r'z\letter-zulu' ]

    def gotResults_mil_spell(self, words, fullResults):
        s = ""
#        print "milword" 
        cap_next = 0
        caps_on = 0
        for word in words:
#            print "milword is [%s]" % word
#            print "word is [%s]" % word
            if re.match(r'\\', word):
#                print 'starts with slash'
                if re.match(r'\\space-bar$', word, re.IGNORECASE):
                    cap_next = 0
                    s = s + ' '
                    continue
                if re.match(r'\\Cap$', word, re.IGNORECASE):
                    cap_next = 1
                    continue
                elif re.match(r'\\Caps-On$', word, re.IGNORECASE):
                    caps_on = 1
                    continue
                elif re.match(r'\\Caps-Off$', word, re.IGNORECASE):
                    caps_on = 0
                    cap_next = 0
                    continue
                elif re.match(r'\\All-Caps-Off', word, re.IGNORECASE):
                    caps_on = 0
                    cap_next = 0
                    continue
                elif re.match(r'\\All-Caps', word, re.IGNORECASE):
                    caps_on = 1
                    continue
                continue
#            print "testing letter"
            letter_match = re.match(r'([A-Za-z])\\', word)
            if letter_match:
                c = letter_match.group(1)
#                print "matches letter %s" % c
                if cap_next or caps_on:
                    c = string.upper(c)
#                print "matches letter %s" % c
                s = s + c
                cap_next = 0
                continue
            letter_match = re.match(r'([0-9])', word)
            if letter_match:
                c = letter_match.group(1)
                s = s + c
                cap_next = 0
                continue

#        print 'milstring "%s"' % s
        self.spelling_cbk(s)

    def activate(self, window):
        """activates the grammar for recognition tied to a window
        with the given handle

        **INPUTS**

        *INT* window -- window handle (unique identifier) for the window

        **OUTPUTS**

        *none*
        """
        if not self.active:
#            print 'activated mil'
            self.active = 1
            self.activateAll(window = window)
    
    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.active:
            self.active = 0
            self.deactivateAll()
    
    def cleanup(self):
        """method which must be called by the owner prior to deleting
        the grammar, to ensure that it doesn't have circular references
        to the owner
        """
        self.deactivate()
        GrammarBase.unload(self)
        MilitarySpelling.cleanup(self)



class SimpleSelectionNL(SimpleSelection, SelectGramBase):
    """natlink implementation of SimpleSelection 

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **attrs):
        self.deep_construct(SimpleSelectionNL,
            {}, attrs, exclude_bases = {SelectGramBase:1})
        SelectGramBase.__init__(self)
        self.load(selectWords = self.select_phrases, throughWord =
            self.through_word)

    def _set_visible(self, visible):
        """internal call to set the currently visible range.

        **INPUTS**

        *STR* visible -- visible text range 

        **OUTPUTS**

        *none*
        """
        SelectGramBase.setSelectText(self, visible)

    def activate(self, window):
        """activates the grammar for recognition tied to the current window,
        and checks with buffer for the currently visible range.

        **INPUTS**

        *INT* window -- window handle (unique identifier) for the window

        **OUTPUTS**

        *none*
        """
        debug.trace('SimpleSelectionNL.activate', 'invoked')
        if not self.is_active():
            SelectGramBase.activate(self, window = window)
            self.active = 1
            self.window = window

    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.is_active():
            SelectGramBase.deactivate(self)
            self.active = 0

    def cleanup(self):
        SelectGramBase.unload(self)
        SimpleSelection.cleanup(self)
    
    def gotBegin(self, moduleInfo):
        debug.trace('SimpleSelectionNL.gotBegin', 'invoked')
        if self.is_active() and moduleInfo[2] == self.window:
            self.recognition_starting()

    def gotResultsObject(self,recogType,resObj):
        debug.trace('SimpleSelectionNL.gotResultsObject', 'invoked')
        if recogType == 'self':
            # If there are multiple matches in the text we need to scan through
            # the list of choices to find every entry which has the highest.
            
            ranges = []        
            try:
                bestScore = resObj.getWordInfo(0)[0][2]
                verb = resObj.getWordInfo(0)[0][0]

                #
                # Collect selection ranges with highest score
                #
                for i in range(100):
                    #
                    # The candidate regions are sorted from best to worst scores.
                    # Loop through candidate regions until we reach one whose
                    # score is not the same as the first score (or until a
                    # natlink.outOfRange exception is raised to signal the end
                    # of the list of candidate regions).
                    #
                    wordInfo = resObj.getWordInfo(i)
                    if not wordInfo:
                        break

                    if wordInfo[0][2] != bestScore:
                        #
                        # All remaining regions don't have as good a score as the
                        # first ones.
                        break
                    else:
                        #
                        # This region has the same score as the first ones. Add it
                        # to the candidate selection ranges.
                        #
                        region = resObj.getSelectInfo(self.gramObj, i)

                        
                        ranges.append(region)

            except natlink.OutOfRange:
                pass

            self.find_closest(verb, ranges)



class TextModeTogglingGramNL(TextModeTogglingGram, GrammarBase):
    """natlink implementation of TextModeToggling

    **INSTANCE ATTRIBUTES**

    *[STR] on_spoken_as* -- list of spoken forms for the command
    that turns text mode on.
        
    *[STR] off_spoken_as* -- list of spoken forms for the command
    that turns text mode off.
             
    *0-1 off_sets_nat_text_to* -- when setting text mode off, put
    nat text into that state.

    **CLASS ATTRIBUTES**

    *none*
    """

    def __init__(self, on_spoken_as, off_spoken_as, 
                       off_sets_nat_text_to,
                       window=None, **attrs):
        debug.trace('TextModeTogglingGramNL.__init__', 'window=%s, attrs=%s' % (window, repr(attrs)))
        self.deep_construct(TextModeTogglingGramNL,
            {'on_spoken_as': None, 
             'off_spoken_as': None, 
              'off_sets_nat_text_to': off_sets_nat_text_to}, 
            attrs, 
            exclude_bases = {GrammarBase:1})
        GrammarBase.__init__(self)
        self.on_spoken_as = map(self.capitalize_rule, on_spoken_as)
        self.off_spoken_as = map(self.capitalize_rule, off_spoken_as)
        self.load(self.gram_spec())

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
        GrammarBase.setExclusive(self, exclusive)


    def configure(on_spoken_as, off_spoken_as, off_sets_nat_text_to):
        #
        # Should change the spoken forms, regenerate the grammar
        # rules, reload them, etc...
        # The gotResults_text_mode_off method should set 
        # nattext to whatever was given in off_sets_nat_text_to
        # 
        debug.not_implemented_yet('TextModeTogglingGramNL.configure')

    def gram_spec(self):
       spec = "<text_mode_on> exported = %s;\n<text_mode_off> exported = %s;" \
              % (compose_alternatives(self.on_spoken_as), compose_alternatives(self.off_spoken_as))
       return spec
    
    def activate(self):
        """activates the grammar for toggling text-mode, tied to the current window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('TextModeTogglingGramNL.activate', 
            'received activate')
        if not self.is_active():
            debug.trace('TextModeTogglingGramNL.activate', 
                'not already active')
            window = self.window
            if window == None:
                window = 0
            GrammarBase.activateAll(self, window = window, 
                exclusive = self.exclusive)
            self.active = 1

            
    def deactivate(self):
        """disable recognition from this grammar

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('TextModeTogglingGramNL.activate', 
            'received deactivate')
        if self.is_active():
            debug.trace('TextModeTogglingGramNL.activate', 
                'was active')
            GrammarBase.deactivateAll(self)
            self.active = 0

    def gotBegin(self, moduleInfo):
        debug.trace('TextModeTogglingGramNL.gotBegin', '** self.exclusive=%s' % self.exclusive)
    
    def gotResults_text_mode_on(self, words, full_results):
        if self.manager:
            self.manager.set_text_mode(1)

    def gotResults_text_mode_off(self, words, full_results):
        if self.manager:
            self.manager.set_text_mode(0)





