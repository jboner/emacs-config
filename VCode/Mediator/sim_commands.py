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

"""mediator simulator commands

Valid commands are:

help()
   display (this) list of commands

open_file(STR fname)
   Opens file with path name *fname* in the editor
   simulator. It also compiles a list of symbols for that file.

compile_symbols([STR] file_list)
   Compiles symbols for all source files in list *file_list*.

clear_symbols()
   Removes all spoken forms of symbols added by VoiceCode to NatSpeak's 
   vocabulary. Spoken forms which consist of a single word are however left
   there.

clear_abbreviations()
   Removes all defined abbreviations from VoiceCode's symbol dictionary


say(STR utterance, never_bypass_sr_recog=0, user_input=None)
   Interprets string *utterance* as though it had been said by a user.

   Note that this command will not work with *Select XYZ* utterances.
   For those, you must use the *say_select* command.
    
   *utterance* can be either:
   
       - a string with the written form of what should be recognised by the
         SR system
         e.g.: say('index not equal to 0')
         
       - a list of words in their written\spoken form (or just
         written if it doesn't have a spoken form different from its
         written form).
         e.g.: say(['index', ' != \\not equal to', '0'])
   

    In general, it's better to specify *utterance* as a list of
    written\spoken words because it allows to simulate exactly what
    the SR does (e.g. what if the SR recognises an LSA as a sequence
    of words instead of its written\spoken form?)

    Note that if the utterance is a *Select XYZ* utterance, the first item
    in list *utterances* should be the whole verb used in the select statement.

    Example: say(['select previous', 'index', ' != \\not equal to', '0'])
             NOT
             say(['select', 'previous', 'index', ' != \\not equal to', '0'])
    
    Argument *user_input* is a string that will be sent to the
    mediator console's standard input. Use in automated regression
    testing, if the *say* command requires user additional user input
    (e.g. confirmation of a symbol match).
     
    If argument *never_bypass_sr_recog* is true, the interpretation will always be done
    through NatLink's recognitionMimic function, even if the 'bypass' switch was on.

goto(INT pos)
   Moves cursor to position *pos*

goto_line(INT linenum)
   Moves cursor to the beginning of line number *linenum*

select(INT start, end)
   Selects from position *start* to position *end* in current buffer

show_buff()
   Prints the content of the current buffer

listen()
   [Unnecessary in gui_sim.py]

   Throws the mediator into a dictation loop. It will listen for
   dictation utterances and interpret and execute any part of the
   utterance that corresponds to a Context Sensitive Command.

   Once in 'listen' mode, you cannot type console commands until you
   have clicked the 'OK' button on the 'Natlink/ Python Subsystem'
   window.

getmic()
    returns current microphone state as a string ('on', 'off', 'sleeping', or
    'disabled')

setmic(state)
    sets current microphone state to the string state ('on', 'off',
    'sleeping' are allowed)

make_position_visible(INT pos)
   Scroll so that pos is visible
   
print_abbreviations(show_unresolved=1)
   Prints out a list of the abbreviations in symbols that were parsed
   so far. If *show_unresolved=1*, also lists unresolved abbreviations
   and the symbols they appear in (an unresolved abbreviation is an
   abbreviation that appeared in a symbol and is neither a speech
   vocabulary word nor a known abbreviation).

print_error(STR message):
   Prints an error to stderr

print_symbols()
   Prints the list of symbols in the known symbols dictionary

provoke()
   causes an error deliberately

save()
   Save the current file

save_as(filename, no_prompt=0)
   Save the current file

quit()
   Quit the simulator.

   Note that if you don't quit using this command
   (e.g. *Ctrl-C*), your DOS window will hang up.   
"""

import mediator_exceptions
import natlink
import os, profile, re, string, sys, time
import vc_globals
from cStringIO import StringIO

from debug import trace

sys.path = sys.path + [vc_globals.config, vc_globals.admin]

from messaging import SocketError
import sr_interface, util, vc_globals
from CSCmd import CSCmd
import Object
import InstanceSpace
# I think we can get by without CmdInterp, EdSim, MediatorObject
# import sr_interface, util, vc_globals

already_ensured_was_in_vocab = {}
def make_sure_word_is_in_vocab(word):
   if not already_ensured_was_in_vocab.has_key(word):
      if sr_interface.getWordInfo(word) == None:
         sr_interface.addWord(word)
   already_ensured_was_in_vocab[word] = 1      


# Set this to 0 during regression testing and > 0 during interactive testing
sleep_before_recognitionMimic = 0

class SimCmdsObj(Object.Object, InstanceSpace.InstanceSpace):
    """an object providing methods corresponding to the non-method
    functions of sim_commands, but without resorting to global
    variables, and in such a way that it can be subclassed to maintain
    compatibility with NewMediatorObject 

    **CLASS ATTRIBUTES**

    *STR help_string* -- the sim_commands help string

    **INSTANCE ATTRIBUTES**

    *AppState app* -- interface to the editor used for interactive or 
    regression testing 

    *CmdInterp interp* -- interface to the mediator's command interpreter 

    *{} names* -- namespace in which to put the quit_flag

    *BOOL* save_speech_files -- Indicates whether or not
    speech files should be saved. If *None*, then ask the user.

    *BOOL* disconnect = 1 -- Indicates whether or not to disconnect from
    the SR system.

    *BOOL* bypass_sr_recog -- Indicates that, for profiling
    purposes, we should bypass natlink for dictation utterances

    *BOOL* testing -- is this SimCmdsObj being used for regression
    testing?

    *BOOL* should_exit -- flag set by NewMediatorObject to tell
    SimCmdsObj that it should raise a CancelTesting exception before the next
    recognitionMimic
    """
    help_string = __doc__
    def __init__(self, app, interp, names, bypass_sr_recog = 0, 
          testing = 0, **args):
        self.deep_construct(SimCmdsObj,
                            {
                             'app': app,
                             'interp': interp,
                             'names': names,
                             'quit_flag': 0,
                             'testing': testing,
                             'save_speech_files' : None,
                             'bypass_sr_recog': bypass_sr_recog,
                             'disconnect_flag': 1,
                             'should_exit': 0
                            }, args, 
                            exclude_bases = {InstanceSpace.InstanceSpace: 1})
    
    def copy_flags(self):
        """copy our internal flags which control the mediator cleanup 
        process to the namespace, in case they are being read from there
        instead of from this object"""
        self.names['save_speech_files'] = self.save_speech_files
        self.names['disconnect_flag'] = self.disconnect_flag

    def bind_methods(self, names):
        """add bound copies of this class's methods to the given dictionary,
        excluding those starting with '_'

        **NOTE:** if you subclass this class, you must redefine this method,
        otherwise if called from an object of the subclass, it will only 
        include methods of the subclass, not this class

        **INPUTS**

        *{STR:ANY} names* -- dictionary into which to insert the  bound
        methods
        """
        my_methods = InstanceSpace.selected_methods(self.__class__,
            exclude = '^_')
#        print my_methods
#        print names
        self.bind_to_space(names, my_methods)
#        print names

    def help(self):
        print self.help_string

    def echo_command(self, cmd_name, *args):
       echo_msg = "Got command: %s(" % cmd_name
       for an_arg in args:
           echo_msg = echo_msg + "%s," % an_arg
       echo_msg = echo_msg + ")"
       print echo_msg

    def open_file(self, fname, echo_cmd=0):
        """Open a file with name in current buffer.

        *STR fname* is the path of the file"""

        if echo_cmd: self.echo_command('open_file', util.within_VCode(fname))
        self.app.open_file(fname)
        self.interp.parse_symbols_from_file(fname)
        self.app.curr_buffer().print_buff_if_necessary()
# show_buff()

    def list_buffers(self, echo_cmd=0):
        if self.echo_cmd: echo_command('list_buffers')
        print self.app.open_buffers_from_app()

    def change_buffer(self, buff_name, echo_cmd=0):
        if echo_cmd: self.echo_command('change_buffer', buff_name)
        if self.app.change_buffer(buff_name):
            self.show_buff()
        else:
            print 'unknown buffer'

    def close_buffer(self, buff_name, save = 0, echo_cmd=0):
        if echo_cmd: self.echo_command('close_buffer', buff_name, save)
        self.app.close_buffer(buff_name, save)

    def save(self, echo_cmd=0):
        """save current buffer"""
        if echo_cmd: self.echo_command('save')
        self.app.save_file()

    def save_as(self, fname, no_prompt = 0, echo_cmd=0):
        """save current buffer

        *STR fname* is the path of the file
        
        *BOOL no_prompt* -- If true, don't prompt before overwriting
        an existing file"""

        if echo_cmd: self.echo_command('save_as', fname, no_prompt)
        self.app.save_file(fname, no_prompt = no_prompt)

    def compile_symbols(self, file_list, echo_cmd=0):
        if echo_cmd: self.echo_command('compile_symbols', file_list)
        self.interp.parse_symbols_from_files(file_list)
        print '>>> Known symbols are: '; self.interp.print_symbols()

    def utterance_spoken_forms(self, utterance, echo_cmd=0):
        spoken_forms = []
        for a_word in utterance:
           spoken, written = sr_interface.spoken_written_form(a_word,
               clean_written = 0, clean_spoken = 0)
           spoken_forms.append(spoken)
        return spoken_forms

        
    def say(self, utterance, user_input=None, never_bypass_sr_recog=0, echo_utterance=0, echo_cmd=0):
        """Simulate an utterance *STR utterance*

        *STR utterance* -- The utterance. This can be a string with the
         written form of what should be recognised by the SR system. If
         it's a list, it should be a list of words in their written\spoken
         form (or just written if it doesn't have a spoken form different
         from its written form).

        In general, it's better to specify *utterance* as a list of
        written\spoken words because it allows to simulate exactly what
        the SR does (e.g. what if the SR recognises an LSA as a sequence
        of words instead of its written\spoken form?)

        *STR user_input* -- A string that will be sent to the mediator console's
         standard input. Use in automated regression testing, if the *say*
         command requires user additional user input (e.g. confirmation of
         a symbol match).
        
        *BOOL echo_utterance=0* -- If true, echo the utterance on STDOUT.
        
        BOOL *never_bypass_sr_recog* -- If *TRUE*, the interpretation will always be done
        through NatLink's recognitionMimic function, even if the 'bypass' switch was on.



        Examples: say('x not equal to') -> 'x != '
                  say(['x', ' != \\not equal to'] -> 'x != '
        """

        
        global sleep_before_recognitionMimic
        
        if self.should_exit:
            trace('SimCmdsObj.say', 'cancelling testing')
            raise mediator_exceptions.CancelTesting()

        if echo_cmd: self.echo_command('say', utterance, user_input, never_bypass_sr_recog, echo_utterance)


#    print 'Saying: %s' % utterance
        sys.stdout.flush()
        if echo_utterance:
            print 'Saying: %s' % utterance

        if user_input:
            #
            # Create temporary user input file
            #
            old_stdin = sys.stdin
            temp_file = None
            if 0:
                temp_file_name = vc_globals.tmp + os.sep + 'user_input.dat'
                temp_file = open(temp_file_name, 'w')
#        print 'temp file opened for writing'
                sys.stdout.flush()
                temp_file.write(user_input)
                temp_file.close()
                temp_file = open(temp_file_name, 'r')
#        print 'temp file opened for reading'
                sys.stdin = temp_file
            else:
                sys.stdin = StringIO(user_input)
            sys.stdout.flush()
            
        try:
            if self.bypass_sr_recog and not never_bypass_sr_recog:
                trace('SimCmdsObj.say', 'bypassing NatSpeak')
                sys.stdout.flush()
                if util.islist(utterance) or util.istuple(utterance):
                    spoken = self.utterance_spoken_forms(utterance)
                else:        
                    utterance = re.split('\s+', utterance)
                    spoken = utterance

                print "Heard %s" % string.join(spoken)
                dictation_allowed = self.app.recog_begin(None)
                self.app.synchronize_with_app()
                buff_name = self.app.curr_buffer_name()
                active_field = self.app.active_field()
                dictation_allowed = dictation_allowed and \
                    (active_field == None)
                if self.testing and not dictation_allowed:
                    trace('SimCmdsObj.say', 'cancelling testing')
                    raise mediator_exceptions.CancelTesting()

                self.interp.interpret_NL_cmd(utterance, self.app)
                self.app.recog_end()
                self.show_buff()        
            else:
                trace('SimCmdsObj.say', 'NOT bypassing NatSpeak')
                if util.islist(utterance) or util.istuple(utterance):
                    words = []
                    #
                    # Clean up the written form in case user didn't type
                    # special characters in the form that the SR expects
                    # (e.g. '\n' instead of '{Enter}'
                    #
                    for a_word in utterance:
                        # Make sure word is in-vocabulary
                        make_sure_word_is_in_vocab(a_word)
# don't want to clean any more
                        spoken, written = sr_interface.spoken_written_form(a_word, 
                            clean_written = 0, clean_spoken = 0)
                        if spoken != written:
# don't want to do this any more
#                        written = sr_interface.clean_written_form(written, clean_for='sr')
                            words = words + [sr_interface.vocabulary_entry(spoken, written)]
                        else:
                            words = words + [written]
                else:        
                    words = re.split('\s+', utterance)
                    for a_word in words:
                        make_sure_word_is_in_vocab(a_word)                    

                trace('SimCmdsObj.say', 'words=%s' % words)
                
#            for word in words:
#                print word, natlink.getWordInfo(word)
#        print '-- mediator.say: words=%s' % words
                sys.stderr.flush()


                #
                # During interactive sessions, may need to pause a few seconds before
                # doing *recognitionMimic*, to give user time to switch to the editor
                # window.
                #
                if sleep_before_recognitionMimic:
                    print '\n\n********************\nPlease click on the editor window before I "say" your utterance.\nYou have %s seconds to do so.\n********************' % sleep_before_recognitionMimic
                    time.sleep(sleep_before_recognitionMimic)

                sys.stderr.flush()

                natlink.recognitionMimic(words)
                sys.stderr.flush()
                if not self.app.alive:
                    trace('SimCmdsObj.say', 'about to raise socket error')
                    sys.stderr.flush()
                    raise \
                        SocketError("socket connection broken during callbacks")
                if self.should_exit:
                    trace('SimCmdsObj.say', 'cancelling testing')
                    sys.stderr.flush()
                    raise mediator_exceptions.CancelTesting()
                
        finally:
            sys.stderr.flush()
            #
            # Redirect stdin back to what it was
            #
            if user_input:
                sys.stdin = old_stdin
                if not (temp_file is None):
                    temp_file.close()

    def goto(self, pos, echo_cmd=0):
        """Goes to position *INT pos* of the current buffer"""
        if echo_cmd: self.echo_command('goto', pos)
        self.app.goto(pos)
        self.show_buff()

    def goto_line(self, linenum, echo_cmd=0):
        """Goes to line number *INT linenum* of current source buffer"""
        if echo_cmd: self.echo_command('goto_line', linenum)
        self.app.goto_line(linenum)
        self.show_buff()

    def goto_end_of_line(self, pos=None, echo_cmd=1):
        """Go to end of the line at a particular cursor position.
        """
        if echo_cmd: self.echo_command('goto_end_of_line', pos)        
        self.app.goto_end_of_line(pos)
        self.show_buff()

    def goto_beginning_of_line(self, pos=None, echo_cmd=1):
        """Go to beginning of the line at a particular cursor position.
        """
        if echo_cmd: self.echo_command('goto_beginning_of_line', pos)
        self.app.goto_beginning_of_line(pos)
        self.show_buff()
        
    def move_relative(self, rel_movement, echo_cmd=0):
        """Goes to position *INT pos* of the current buffer"""
        if echo_cmd: self.echo_command('move_relative', rel_movement)
        self.app.move_relative(rel_movement)
        self.show_buff()        

    def make_position_visible(self, pos, echo_cmd=0):
        if echo_cmd: self.echo_command('make_position_visible', pos)
        self.app.make_position_visible(pos)
        self.show_buff()

    def select(self, start, end, echo_cmd=0):
        """Selects from position *start* to position *end* in current buffer"""
        if echo_cmd: self.echo_command('select', start, end)
        self.app.set_selection((start, end))
        self.show_buff()
        
    def show_buff(self, echo_cmd=0):
        """Shows content of current source buffer"""
        if echo_cmd: self.echo_command('show_buff')
        self.app.curr_buffer().print_buff_if_necessary()

    def move(self, steps, echo_cmd=0):
        """Moves cursor by *INT steps* (can be negative)"""
        if echo_cmd: self.echo_command('move', steps)
        self.app.move_relative(steps)
        self.show_buff()

    def listen(self, echo_cmd=0):
        if echo_cmd: self.echo_command('listen')
        natlink.setMicState('on')
        natlink.waitForSpeech(0)
        natlink.setMicState('off')

    def print_error(self, message, echo_cmd=0):
        if echo_cmd: self.echo_command('print_error', message)
        sys.stderr.write(message)

    def provoke(self, echo_cmd=0):
        if echo_cmd: self.echo_command('provoke')
        print slidjf

    def print_symbols(self, symbols = None, echo_cmd=0):
        if echo_cmd: self.echo_command('print_symbols')
        self.interp.known_symbols.print_symbols(symbols = symbols)

    def print_abbreviations(self, show_unresolved=1, echo_cmd=0):
        if echo_cmd: self.echo_command('print_abbreviations', show_unresolved)
        self.interp.known_symbols.print_abbreviations(show_unresolved)

    def clear_symbols(self, echo_cmd=0):
        if echo_cmd: self.echo_command('clear_symbols')
        #
        # Remove symbols from the Speech Recognition vocabulary
        #
        self.interp.cleanup_dictionary()

    def clear_abbreviations(self, echo_cmd=0):
        if echo_cmd: self.echo_command('clear_abbreviations')
        #
        # Remove abbreviations from the symbol dictionary
        #
        self.interp.abbreviations_cleanup()

    def signal_quitting(self, quitting = 1, echo_cmd=0):
        if echo_cmd: self.echo_command('signal_quitting', quitting)
        self.names['quit_flag'] = quitting
        self.copy_flags()

    def quit(self, save_speech_files=None, disconnect=1, echo_cmd=0):
        if echo_cmd: self.echo_command('quit', save_speech_files, disconnect)
        self.save_speech_files = save_speech_files
        self.disconnect_flag = disconnect
        self.signal_quitting()

    def getmic(self, echo_cmd=0):
        if echo_cmd: self.echo_command('getmic')
        return sr_interface.get_mic()

    def setmic(self, state, echo_cmd=0):
        if echo_cmd: self.echo_command('setmic', state)
        sr_interface.set_mic(state)
