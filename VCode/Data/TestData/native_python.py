import os, re, string, sys

import auto_test, natlink, vc_globals
from actions_C_Cpp import *
from actions_py import *
from AppState import AppState
from cont_gen import ContC, ContPy
from CSCmd import CSCmd
from EdSim import EdSim
from Object import Object
import EdSim, SymDict
import sr_interface

class CmdInterp(Object):
    """Interprets Context Sensitive Commands spoken into a given application.
    
    **INSTANCE ATTRIBUTES**

    [AppState] *on_app=None* -- application for which we are
    interpreting the commands
    
    *{STR: [[* (Context] *, FCT)]} cmd_index={}* -- index of CSCs. Key
     is the spoken form of the command, value is a list of contextual
     meanings. A contextual meaning is a pair of a *context object*
     and an *action function* to be fired if the context applies.

    *[SymDict] known_symbols* -- dictionary of known symbols
    
    *{STR: [STR]}* language_specific_aliases = {} -- Key is the name of
     a programming language (None means all languages). Value is a
     list of written form\spoken form words specific to a
     language. These words are loaded automatically when we are in a
     source buffer of that language and removed when we change to a
     buffer in a different language.

    *STR* last_loaded_language = None -- Name of the previous language
     for which the language specific words were loaded.

    *FILE symdict_pickle_file = None* -- File used to for
     reading/writing the symbol dictionary. If *None*, then don't
     read/write the symbol dictionary from/to file.

    *INT* _untranslated_text_start = None -- Start position of the
     current string of untranslated text inserted in current buffer.

    *INT* _untranslated_text_end = None -- End position of the
     current string of untranslated text inserted in current buffer.
     
    
    CLASS ATTRIBUTES**

    *none* --
        
    .. [AppState] file:///./AppState.AppState.html
    .. [Context] file:///./Context.Context.html
    .. [SymDict] file:///./SymDict.SymDict.html"""
    
    def __init__(self, on_app=None, symdict_pickle_file=None, **attrs):

        #
        # These attributes can't be set at construction time
        #
        self.decl_attrs({'_untranslated_text_start': None, '_untranslated_text_end': None})

        #
        # But these can
        #
        self.deep_construct(CmdInterp,
                            {'on_app': on_app, 'cmd_index': {}, \
                             'known_symbols': SymDict.SymDict(), \
                             'language_specific_aliases': {},\
                             'last_loaded_language': None, \
                             'symdict_pickle_file': symdict_pickle_file},\
                            attrs)


    def spoken_form_regexp(self, spoken_form):
        """Returns a regexp that matches a spoken form of a command.

        *STR spoken_form* is the spoken form. The returned regexp will match
        it even if the case of the first letter of each word do not match."""

        words = re.split('\s+', spoken_form)
        regexp = ''
        for aword in words:
            first = aword[0]
            rest = aword[1:]
            regexp_this_word = '[' + string.lower(first) + string.upper(first) + ']' + rest
            if not regexp == '':
                regexp = regexp + '\s*'
            regexp = regexp + regexp_this_word
        return regexp

    def interpret_NL_cmd(self, cmd):
        
        """Interprets a natural language command and executes
        corresponding instructions.

        *[STR] cmd* -- The command. It is a list of written\spoken words.
        """
        
        print '-- CmdInterp.interpret_NL_cmd: cmd=%s' % cmd

        self._untranslated_text_start = None
        self._untranslated_text_end = None

        cmd = self.massage_command(cmd)
        
        #
        # Process the beginning of the command until there is nothing
        # left
        #
        while len(cmd) > 0:
             print '-- CmdInterp.interpret_NL_cmd: now, cmd=%s' % cmd
             print '-- CmdInterp.interpret_NL_cmd: cur_pos=%s' % self.on_app.curr_buffer.cur_pos; self.on_app.print_buff_content()

             #
             # Identify leading CSC, LSA, symbol and ordinary word
             #
             chopped_CSC, CSC_consumes, cmd_without_CSC = self.chop_CSC(cmd)
             chopped_LSA, LSA_consumes, cmd_without_LSA = self.chop_LSA(cmd)
             chopped_symbol, symbol_consumes, cmd_without_symbol = self.chop_symbol(cmd)
             chopped_word, word_consumes, cmd_without_word = self.chop_word(cmd)             
             most_consumed = max((LSA_consumes, symbol_consumes, CSC_consumes, word_consumes))

             print '-- CmdInterp.interpret_NL_cmd: chopped_CSC=%s, CSC_consumes=%s, chopped_LSA=%s, LSA_consumes=%s, chopped_symbol=%s, symbol_consumes=%s, chopped_word=%s, word_consumes=%s' % (chopped_CSC, CSC_consumes, chopped_LSA, LSA_consumes, chopped_symbol, symbol_consumes, chopped_word, word_consumes)
             head_was_translated = 0

             #
             # Translate CSC, LSA, symbol or ordinary word at head of command.
             #
             # If more than one translations are possible, choose the one
             # that consumes the most words from the command.
             #
             # In case of ties, use this order of priority: CSC, LSA, symbol,
             # ordinary words. This order goes from most specific to least
             # specific, i.e.
             #
             # - CSCs usually apply in very restricted contexts only
             # - LSAs usually apply for a specific language only
             # - Symbols are restricted to sequences of words that are the
             #   spoken form of a known symbol
             # - ordinary words can be anything
             #
             
             if CSC_consumes == most_consumed:
                 #
                 # CSC consumed the most words from the command.
                 # Try all the CSCs with this spoken form until find
                 # one that applies in current context
                 #
                 print '-- CmdInterp.interpret_NL_cmd: processing leading CSC=\'%s\'' % chopped_CSC
                 CSCs = self.cmd_index[chopped_CSC]
                 csc_applied = 0
                 for aCSC in CSCs:
                     csc_applied = aCSC.interpret(self.on_app)
                     if (csc_applied):
                         break
                 if csc_applied:
                     #
                     # Found a CSC that applies
                     # Chop the CSC from the command
                     #
                     cmd = cmd_without_CSC
                     head_was_translated = 1
                     print '-- CmdInterp.interpret_NL_cmd: after translating CSC, buffer is:'; self.on_app.print_buff_content()                                                  
                 else:
                     #
                     # As it turns out, none of the CSCs with this
                     # spoken form apply in current context
                     # So don't chop the CSC
                     #
                     chopped_CSC = None
                     CSC_consumes = 0
                     most_consumed = max((LSA_consumes, symbol_consumes, word_consumes))
             
             if not head_was_translated and LSA_consumes == most_consumed:
                 #
                 # LSA consumed the most words from command. Insert it.
                 #
                 print '-- CmdInterp.interpret_NL_cmd: processing leading LSA=\'%s\'' % chopped_LSA
                 self.on_app.insert_indent(chopped_LSA, '')
                 cmd = cmd_without_LSA
                 head_was_translated = 1
                 print '-- CmdInterp.interpret_NL_cmd: after translating LSA, buffer is:'; self.on_app.print_buff_content()                 


             if not head_was_translated and symbol_consumes == most_consumed:
                 #
                 # Symbol consumed the most words from command. Insert it.
                 #
                 # Note: known symbols are inserted as untranslated
                 #       text because often, the user will create new
                 #       symbols by prefixing/postfixing existing ones.
                 #       For example, if you define a subclass of a known
                 #       class SomeClass you may name the new class
                 #       SomeprefixSomeClass or SomeClassSomepostfix.
                 #
                 print '-- CmdInterp.interpret_NL_cmd: processing leading symbol=\'%s\'' % chopped_symbol                     
                 self.insert_untranslated_text(chopped_symbol)
                 cmd = cmd_without_symbol
                 head_was_translated = 1
                 print '-- CmdInterp.interpret_NL_cmd: after translating symbol, buffer is:'; self.on_app.print_buff_content()                     
                                          
                    
             if not head_was_translated and word_consumes == most_consumed:
                 #
                 # Nothing special translated at begining of command.
                 # Just chop off the first word and insert it, marking
                 # it as untranslated text.
                 #                 
                 print '-- CmdInterp.interpret_NL_cmd: processing leading word=\'%s\'' % chopped_word                                                  
                 self.insert_untranslated_text(chopped_word)
                 cmd = cmd_without_word
                 head_was_translated = 1
                 print '-- CmdInterp.interpret_NL_cmd: after translating ordinary word, buffer is:'; self.on_app.print_buff_content()

             #
             # Finished translating head of command.
             #
             # Check if it marked the end of some untranslated text
             #
             if (CSC_consumes or LSA_consumes or len(cmd) == 0) and \
                self._untranslated_text_start != None:
                #
                # A CSC or LSA was translated, or we reached end of the
                # command, thus marking the end of a sequence of untranslated
                # text. Try to match untranslated text to a known (or new)
                # symbol.
                #
                self.match_untranslated_text()

             if self._untranslated_text_start != None:
                 untranslated_text = self.on_app.curr_buffer.content[self._untranslated_text_start:self._untranslated_text_end]
             else:
                 untranslated_text = None
             print '-- CmdInterp.interpret_NL_cmd: End of *while* iteration. untranslated_text=\'%s\', self._untranslated_text_start=%s, self._untranslated_text_end=%s, self.on_app.curr_buffer.cur_pos=%s' % (untranslated_text, self._untranslated_text_start, self._untranslated_text_end, self.on_app.curr_buffer.cur_pos)



    def massage_command(self, command):
        """Massages a command to prepare it for interpretation.

        Makes sure to substitute special characters (e.g. {Spacebar})
        in the written form of words in the command. Also, makes sure
        that the spoken forms are all lowercase, and contain no
        multiple, leading or trailing blanks.
        
        **INPUTS**
        
        *[STR]* command -- The command to be massaged. It's a list of
         written\spoken words.
        
        **OUTPUTS**
        
        *[STR] mod_command* -- The massaged command
        """
        
        mod_command = []
        for a_word in command:
            spoken, written = sr_interface.spoken_written_form(a_word)
            written = sr_interface.clean_written_form(written, clean_for='vc')
            spoken = re.sub('\s+', ' ', spoken)
            spoken = re.sub('(^\s+|\s+$)', '', spoken)
            mod_command = mod_command + [sr_interface.vocabulary_entry(spoken, written, clean_written=0)]
        return mod_command

                                
    def insert_untranslated_text(self, text):
        
        """Inserts some text in current buffer, and marks it as untranslated
        text.

        The next time a CSC or LSA is encountered, the interpreter will try
        to match that text (and all untranslated text that immediatly
        precedes/follows it) to a new symbol, or a known symbol with
        unresolved spoken forms.
        
        **INPUTS**
        
        *STR* text -- The text to be inserted
        

        **OUTPUTS**
        
        *none* -- 
        """

#        print '-- CmdInterp.insert_untranslated_text: text=\'%s\',  ' % (text,  )

        if self._untranslated_text_start != None:
            #
            # This is inserted in middle of an untranslated region.
            # Insert space between this word and the one that preceded
            # it in the untranslated region.
            #
            text = ' ' + text
            
        self.on_app.insert_indent(text, '')            

        if self._untranslated_text_start == None:
            #
            # This was the beginning of a sequence of
            # untranslated text. Remember its start
            # position.
            #
            # NOTE: must take care to set start past any blanks that may
            # have been inserted for indentation
            #
            self._untranslated_text_start = self.on_app.curr_buffer.cur_pos - len(text)            
        
        #
        # Remember end of the current sequence of untranslated
        # text.
        #
        self._untranslated_text_end = self.on_app.curr_buffer.cur_pos

#        print '-- CmdInterp.insert_untranslated_text: self._untranslated_text_start=%s, self._untranslated_text_end=%s, untranslated region=\'%s\'' % (self._untranslated_text_start, self._untranslated_text_end, self.on_app.curr_buffer.content[self._untranslated_text_start:self._untranslated_text_end]); self.on_app.print_buff_content()
        

    def match_untranslated_text(self):
        """Tries to match last sequence of untranslated text to a symbol.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        untranslated_text = self.on_app.curr_buffer.content[self._untranslated_text_start:self._untranslated_text_end]

        print '-- CmdInterp.match_untranslated_text: untranslated_text=%s, self._untranslated_text_start=%s, self._untranslated_text_end=%s, untranslated region=\'%s\'' % (untranslated_text, self._untranslated_text_start, self._untranslated_text_end, self.on_app.curr_buffer.content[self._untranslated_text_start:self._untranslated_text_end]); self.on_app.print_buff_content()        

        a_match = re.match('(\s*)([\s\S]*)\s*$', untranslated_text)
        text_no_spaces = a_match.group(2)
        leading_spaces = a_match.group(1)

#        print '-- CmdInterp.match_untranslated_text: text_no_spaces=\'%s\', leading_spaces=\'%s\'' % (text_no_spaces, leading_spaces)
                
        #
        # Match untranslated text to new known symbol or a known symbol with
        # unresolved spoken forms.
        #
        # Don't bother the user if the untranslated text is just the written
        # form of a known symbol or if it's a number
        #
        reg = '[\d\s]+'
#        print '-- CmdInterp.match_untranslated_text: reg=%s' % reg
        num_match = re.match(reg, text_no_spaces)
        if not self.known_symbols.symbol_info.has_key(text_no_spaces) and \
           not num_match:
#        print '-- CmdInterp.match_untranslated_text: trying to match untranslated text to a symbol. untranslated_text=\'%s\', self._untranslated_text_start=%s, self._untranslated_text_end=%s' % (untranslated_text, self._untranslated_text_start, self._untranslated_text_end)
            symbol_matches = self.known_symbols.match_pseudo_symbol(untranslated_text)
            if symbol_matches:
                self.dlg_select_symbol_match(symbol_matches)

        #
        # Now, there is no more untranslated text.
        #
        self._untranslated_text_start = None
        self._untranslated_text_end = None
        

    def dlg_select_symbol_match(self, symbol_matches):
        """Asks the user to select a match for pseudo symbol.
        
        **INPUTS**
        
        *[SymbolMatch]* symbol_matches -- List of possible matches.
        

        **OUTPUTS**
        
        *none* -- 

        .. [SymbolMatch] file:///./SymDict.SymbolMatch.html"""

        untranslated_text = self.on_app.curr_buffer.content[self._untranslated_text_start:self._untranslated_text_end]

        good_answer = 0
        while not good_answer:
            print 'Associate \'%s\' with symbol (Enter selection):\n' % untranslated_text
            print '  \'0\': no association'
            ii = 1
            for a_match in symbol_matches:
                sys.stdout.write('  \'%s\': %s' % (ii, a_match.native_symbol))
                if a_match.is_new:
                    sys.stdout.write(' (*new*)')
                sys.stdout.write('\n')
                ii = ii + 1
#                print '-- CmdInterp.dlg_select_symbol_match: a_match.score()=%s, a_match.__dict__=%s' % (a_match.score(), a_match.__dict__)
            sys.stdout.write('\n> ')
            answer = sys.stdin.readline()
#            print '-- CmdInterp.dlg_select_symbol_matches: answer=\'%s\'' % answer
            answer_match = re.match('\s*([\d])+\s*', answer)
            if answer_match:
                choice_index = int(answer_match.group(1)) - 1
                if choice_index < len(symbol_matches) and choice_index >= -1:

                    good_answer = 1
            if not good_answer:
                print 'Invalid answer \'%s\'' % answer

        #
        # Accept the match
        #
        if choice_index >= 0:
            #
            # A match was chosen. Accept it and type it instead of the
            # untranslated text.
            #
            chosen_match = symbol_matches[choice_index]
            self.known_symbols.accept_symbol_match(chosen_match)

            #
            # Insert matched symbol, then go back to where we were
            #
            old_pos = self.on_app.curr_buffer.cur_pos
            self.on_app.insert_indent(chosen_match.native_symbol, '', start=self._untranslated_text_start, end=self._untranslated_text_end)
            new_pos = old_pos + len(chosen_match.native_symbol) - (self._untranslated_text_end - self._untranslated_text_start)
            self.on_app.move_to(new_pos)
            

    def chop_CSC(self, cmd):
        """Chops the start of a command if it starts with a CSC.
        
        **INPUTS**
        
        *[STR]* cmd -- The command. It is a list of written\spoken words.

        **OUTPUTS**

        Returns a tuple *(chopped_CSC, consumed, rest)* where:
        
        *STR* chopped_symbol -- The written form of the CSC that
        was chopped off. If *None*, it means *cmd* did
        not start with a known CSC.

        *INT* consumed* -- Number of words consumed by the CSC from
         the command

        *[STR]* rest -- is what was left of *cmd* after the CSC
         was chopped off.        
        """
        
#        print '-- CmdInterp.chop_CSC: cmd=%s' % cmd

        chopped_CSC, consumed, rest = None, 0, cmd

        #
        # Create list of spoken forms of the words in command
        #
        words = []
        for a_word in cmd:
            a_word, dummy = sr_interface.spoken_written_form(a_word)
            words = words + [a_word]

        #
        # Starting with the whole command and dropping words from the end,
        # check if that corresponds to the spoken form of a known CSC.
        #
        upto = len(words)
        while upto:
            a_spoken_form = string.join(words[:upto], ' ')
#            print '-- CmdInterp.chop_CSC: upto=%s, a_spoken_form=%s' % (upto, a_spoken_form)

            if self.cmd_index.has_key(a_spoken_form):
                #
                # This corresponds to the spoken form of a CSC and it's the
                # longest one we'll ever find.
                #
#                print '-- CmdInterp.chop_CSC: matches known CSC \'%s\'' % a_spoken_form
                chopped_CSC = a_spoken_form
                rest = cmd[upto:]
                consumed = upto
                break
            upto = upto - 1
        return chopped_CSC, consumed, rest


    def chop_LSA(self, command):
        """Chops off the first word of a command if it is an LSA.
                
        **INPUTS**
        
        *[STR]* command -- The command. It is a list of words in
         written\spoken form.        

        **OUTPUTS**
        
        Returns a tuple *(chopped_LSA, consumed, rest)* where:
        
        *STR* chopped_LSA -- The written form of the LSA that was
         chopped off. If *None*, it means *command* did not start with
         an LSA.

        *INT* consumed* -- Number of words consumed by the LSA from
         the command (always 1, but return it anyway because want to
         keep same signature as chop_CSC and chop_symbol)

        *[STR]* rest -- is what was left of *command* after the LSA
         was chopped off.
        """
        
        print '-- CmdInterp.chop_LSA: command=%s' % command
        
        chopped_LSA, consumed, rest = None, 0, command
        leading_word = command[0]

        #
        # Check if the LSA is in the list of LSAs for the current
        # language, or in the list of non-language specific aliases
        # (i.e. language=None)
        #
        active_language_LSAs = []
        if self.language_specific_aliases.has_key(self.on_app.active_language()):
            active_language_LSAs = self.language_specific_aliases[self.on_app.active_language()]
        all_language_LSAs = []
        if self.language_specific_aliases.has_key(None):
            all_language_LSAs = self.language_specific_aliases[None]

        print '-- CmdInterp.chop_LSA: leading_word=%s, active_language_LSAs=%s, all_language_LSAs=%s' % (leading_word, active_language_LSAs, all_language_LSAs)
        if (leading_word in active_language_LSAs) or \
           (leading_word in all_language_LSAs):
            chopped_LSA = leading_word
            rest = command[1:]
            consumed = 1

#        print '-- CmdInterp.chop_LSA: returning chopped_LSA=%s, rest=%s' % (chopped_LSA, rest)
        return chopped_LSA, consumed, rest


    def chop_symbol(self, command):
        """Chops off the beginning of a command if it is a known symbol.
        
        **INPUTS**
        
        *[STR]* command -- The command. It is a list of written\spoken words.

        **OUTPUTS**

        Returns a tuple *(chopped_symbol, consumed, rest)* where:
        
        *STR* chopped_symbol -- The written form of the known symbol that
        was chopped off. If *None*, it means *command* did
        not start with a known symbol.

        *INT* consumed* -- Number of words consumed by the symbol from
         the command

        *[STR]* rest -- is what was left of *command* after the symbol
         was chopped off.        
        """

#        print '-- CmdInterp.chop_symbols: command=%s' % command

        chopped_symbol, consumed, rest = None, 0, command

        #
        # Create list of spoken forms of the words in command
        #
        words = []
        for a_word in command:
            a_word, dummy = sr_interface.spoken_written_form(a_word)
            words = words + [a_word]

        #
        # Starting with the whole command and dropping words from the end,
        # check if that corresponds to a known symbol.
        #
        upto = len(words)
        while upto:
            a_spoken_form = string.join(words[:upto], ' ')
#            print '-- CmdInterp.chop_symbols: upto=%s, a_spoken_form=%s' % (upto, a_spoken_form)
            if self.known_symbols.spoken_form_info.has_key(a_spoken_form):
                # This corresponds to the spoken form of a symbol
#                print '-- CmdInterp.chop_symbols: matches a known symbol'
                chopped_symbol = self.choose_best_symbol(a_spoken_form, self.known_symbols.spoken_form_info[a_spoken_form].symbols)
                rest = command[upto:]
                consumed = upto
                break
            upto = upto - 1
        return chopped_symbol, consumed, rest



    def chop_word(self, command):
        """Removes a single word from a command.
        
        **INPUTS**
        
        *[STR]* command -- The command. It is a list of written\spoken words.
        

        **OUTPUTS**
        
        Returns a tuple *(chopped_word, consumed, rest)* where:

        *STR* chopped_word -- The spoken form of the first word

        *INT* consumed -- Number of words consumed (always 1, but
         return it anyway because want to keep same method signature
         as chop_CSC, chop_LSA and chop_symbol).

        *[STR]* rest -- Rest of the command after the word was chopped
        
        """
        
        chopped_word, dummy = sr_interface.spoken_written_form(command[0])
        consumed = 1
        rest = command[1:]
        return chopped_word, consumed, rest
        

    def choose_best_symbol(self, spoken_form, choices):
        """Chooses the best match for a spoken form of a symbol.

        For now, we just choose the first item in *choices*, but in
        the future, we might choose the one that appears closest to
        the cursor, or the one that used most recently, or the one
        that best matches the spoken form.
        
        **INPUTS**
        
        *STR* spoken_form -- spoken form of the symbol. 
        
        *ANY* choices -- undocumented 
        

        **OUTPUTS**
        
        *none* -- 
        """

        return choices[0]

    def index_csc(self, acmd, add_voc_entry=1):
        """Add a new csc to the command interpreter's command dictionary

        [CSCmd] *acmd* is the command to be indexed.

        *BOOL add_voc_entry = 1* -- if true, add a SR vocabulary entry
         for the CSC's spoken forms

        .. [CSCmd] file:///./CSCmd.CSCmd.html"""

        global regexp_is_dirty

        print '-- CmdInterp.index_csc: acmd.spoken_forms=\'%s\', add_voc_entry=%s' % (acmd.spoken_forms, add_voc_entry)
        regexp_is_dirty = 1

        for a_spoken_form in acmd.spoken_forms:
            #
            # Remove leading, trailing and double blanks from the spoken form
            #
            re.sub('\s+', ' ', a_spoken_form)
            re.sub('^\s+', '', a_spoken_form)
            re.sub('\s+$', '', a_spoken_form)
            a_spoken_form = string.lower(a_spoken_form)

            #
            # Index the spoken form
            #
            if (self.cmd_index.has_key(a_spoken_form)):
                #
                # Already indexed. Just add to the list of CSCs for that
                # spoken form
                #
#                print '-- CmdInterp.index_csc: spoken form \'%s\'already indexed. Not adding to SR vocabulary' % a_spoken_form
                cmds_this_spoken_form = self.cmd_index[a_spoken_form]
                cmds_this_spoken_form[len(cmds_this_spoken_form):] = [acmd]
            else:
                #
                # First time indexed. Create a new list of CSCs for that
                # spoken form, and add it to the SR vocabulary.
                #
#                print '-- CmdInterp.index_csc: spoken form \'%s\' indexed for first time. Adding to SR vocabulary' % a_spoken_form
                self.cmd_index[a_spoken_form] = [acmd]
                if not os.environ.has_key('VCODE_NOSPEECH') and add_voc_entry:
                    sr_interface.addWord(a_spoken_form)



    def load_language_specific_aliases(self):
        
        """Loads words specific to the language of the current buffer,
        if needed.

        Also, unloads words specific to previously loaded language if needed.
        
        **INPUTS**
        
        *none*

        **OUTPUTS**
        
        *none* -- 
        """

        language = self.on_app.active_language()
        last_language = self.last_loaded_language
#        print '-- CmdInterp.load_language_specific_aliases: called, language=%s, last_language=%s' % (language, last_language)
#        print '-- CmdInterp.load_language_specific_aliases: self.language_specific_aliases[%s]=%s' % (language, self.language_specific_aliases[language])
        if language != last_language:
            #
            # Remove words from previous language (unless it was None
            # which means LSAs not specific to a particular language)
            #
            if last_language != None and \
               self.language_specific_aliases.has_key(last_language):
                for a_word in self.language_specific_aliases[last_language]:
                    sr_interface.deleteWord(a_word)
                    spoken_as, written_as = sr_interface.spoken_written_form(a_word)
            
            #
            # Add words for new language (unless language == None, which
            # means LSAs that are not language specific)
            #
            if language != None and \
               self.language_specific_aliases.has_key(language):
                for a_word in self.language_specific_aliases[language]:
                    sr_interface.addWord(a_word)
                    spoken_as, written_as = sr_interface.spoken_written_form(a_word)

            self.last_loaded_language = language
            
#        print '-- CmdInterp.load_language_specific_aliases: finished'



