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
"""Interface to the Speech Recognition engine.
"""

import re
import string
import natlink
from natlinkutils import *
import debug
from debug import trace, trace_call_stack

import SpokenUtterance


#import nsformat

#
# Flag that remembers whether or not we have connected to NatSpeak
#
sr_is_connected = 0

#
# microphone state change callback (None for no callback
#
#    *FCT* mic_change_callback -- 
#      mic_change_callback(*STR* mic_state)
#    (optional) function to be called when the microphone state changes.

sr_mic_change_callback = None

#
# Flag that remembers whether or not the VoiceCode SR user was modified since
# last saved
#
sr_user_needs_saving = 0

# maximum word length for getWordInfo:
sr_max_word_length = 200
#
# Name, base model and base topic to use for VoiceCode
#
vc_user_name = 'VoiceCode'
vc_base_model = 'BestMatch Model'
vc_base_topic = 'US General English - BestMatch Plus'


#
# Flag used to add a word to NatSpeak
# 0x40000000 -> added by VoiceCode (i.e. alias Vocabulary Builder)
# 0x00000001 -> added by user
#
#
# Words added by VoiceCode to NatSpeak's vocabulary can be added either
# as 'user' or as 'vocabulary builder'.
#
# If added as 'user', then the a-priori probabibility of the words is very
# high which may improve accuracy for coding, but decrease accuracy for
# regular dictation. Also, words added by VoiceCode are indistinguishable from
# words added by the user.
#
# If added as 'vocabulary builder', a-priori probability of the words is
# low, which decreases accuracy for programming, but doesn't affect accuracy
# of regular dictation. However, this makes the symbols distinguishable from
# user added symbols (but not distinguishable from vocabulary builder added
# symbol, but that doesn't matter because we only remove multi word vocabulary
# entries, and vocabulary builder entries are all single words.
#
add_words_as = 'user'
if add_words_as == 'user':
    word_info_flag = 0x00000001
else:
    word_info_flag = 0x40000000


def set_mic(mic_state):
    """turns microphone on or off (connecting first, if necessary).

    *STR* mic_state -- *'on'* or *'off'*. State to put the mic in
    after connection.
    """

#    if not sr_is_connected:
#        connect()
    natlink.setMicState(mic_state)

def get_mic():
    """checks the current microphone state

    *STR* mic_state -- *'on'* or *'off'*. State to put the mic in
    after connection.
    """
#    if not sr_is_connected:
#        connect()
    return natlink.getMicState()


def connect(user_name, mic_state=None, mic_change_callback = None):
    """Connects to the SR system.
    
    **INPUTS**
    
    *STR* name of the Natspeak user to open

    *STR* mic_state -- *'on'* or *'off'*. State to put the mic in
    after connection. If *None*, then leave mic alone.

    *FCT* mic_change_callback -- 
      mic_change_callback(*STR* mic_state)
    (optional) function to be called when the microphone state changes.
    
    **OUTPUTS**
    
    *none* -- 
    """
    global sr_is_connected, vc_user_name, vc_base_model, vc_base_topic
    global sr_mic_change_callback
    
    
    if not sr_is_connected:
        trace('sr_interface.connect', 'mic_state=%s, sr_is_connected=%s' % (mic_state, sr_is_connected))
#        trace_call_stack('sr_interface.connect')
# 1 means use threads -- needed for GUI apps
        natlink.natConnect(1)
        sr_is_connected = 1            
        openUser(user_name)
    if mic_state:
        natlink.setMicState(mic_state)
    if mic_change_callback:
        set_change_callback(mic_change_callback)


def disconnect():
    """Dicsconnects from SR system.
    
    **INPUTS**
    
    *none* -- 
    
    
    **OUTPUTS**
    
    *none* -- 
    """
    global sr_is_connected, sr_mic_change_callback

    if sr_mic_change_callback:
        natlink.setChangeCallback(None)
        sr_mic_change_callback = None
    natlink.natDisconnect()
    sr_is_connected = 0        


def set_change_callback(mic_change_callback):
    global sr_mic_change_callback
    sr_mic_change_callback = mic_change_callback
    natlink.setChangeCallback(mic_change_callback)

def openUser(user_name):

    """Open a user
        
    **INPUTS**
        
    *STR* user_name -- Name of the user to be opened.

    **OUTPUTS**
        
    *none* -- 
    """
    
    natlink.openUser(user_name)

def saveUser():
    """Saves the current user"""
    natlink.saveUser()
    sr_user_needs_saving = 0

marking_word = r'VoiceCode\Deleting this word will cause voice code to ' + \
            're-scan its standard symbol files'

def mark_user():
    """marks the current user as having been used with VoiceCode

    Note: this function is used to detect when standard symbol files should 
    be re-scanned, because a fresh VoiceCode user file has been created.
    It should not be called except by SymDict's finish_config method.
    """
    addWord(marking_word)

def is_user_marked():
    """checks whether the current user is marked as having been used with 
    VoiceCode

    Note: this function is used to detect when standard symbol files should 
    be re-scanned, because a fresh VoiceCode user file has been created
    """
    #
    # Note: getWordInfo doesn't process the word through spoken_written_form
    # anymore.
    if getWordInfo(marking_word) is None:
        return 0
    return 1


def send_keys(s, system = 0):
    """simulates keystroke input
    
    **INPUTS**
    
    *STR s* -- keys to send, in Dragon (DDWIN/Natspeak scripting
    language (pre-v6)) notation

    *BOOL system* -- if true, send the keystrokes as system keys
    (necessary for proper interpretation of Alt+Tab, windows hot keys,
    etc.)

    **OUTPUT**

    *none*
    """
    flags = 0
    if system:
        flags = hook_f_systemkeys
    natlink.playString(s, flags)

def addedByVC(flag):   
    """Returns *true* iif word information *flag* indicates that word
    was added by VoiceCode"""

#    trace('sr_interface.addedByVC', 'flag=%s' % str(flag))
    if flag == None:
        indicator = 0
    elif (add_words_as == 'user'):
        indicator = (flag % int(0x00000010))        
    else:
        indicator = (flag / int(0x40000000))
#    trace('sr_interfacen.addedByVC', 'indicator=%s' % indicator)
    return indicator

def getWordInfo(word, flag = None):
    """get word info, if word OK
    
    skip if there is no spoken form, or the length > sr_max_word_length
    """
##    trace('sr_interface.getWordInfo', 'word=%s, rest=%s' % (word, rest))
    
    if not word:
        trace('sr_interface.getWordInfo', 'getWordInfo, empty word: %s'% word)
        return
    if word.endswith('\\'):
        trace('sr_interface.getWordInfo', 'getWordInfo, empty spoken form: %s'% word)
        return
    if len(word) > sr_max_word_length:
        trace('sr_interface.getWordInfo', 'getWordInfo, (too?) long word (%s):\n%s'% (len(word), word))
        return
        
    try:
       if flag is None:
           answer = natlink.getWordInfo(word)
       else:
           answer = natlink.getWordInfo(word, flag)
    except:
       # In case the word's spelling is not allowed by
       # NatSpeak
       print "WARNING: error trying to get info from vocabulary word '%s'\n" + \
             "Maybe you forgot to start Dragon NaturallySpeaking before starting VoiceCode?" % repr(word)
#       debug.print_call_stack()
       answer = None       

    return answer

def word_exists(word):
    """check if a word exists

    **INPUTS**

    *STR* word -- the word
    """
    word_info = getWordInfo(word)    
    if word_info  is None:    
        return 0
    return 1

def addWord(word, *rest):
    """Add a word to NatSpeak's vocabulary.

    We only add the word if it doesn't already exist in the vocabulary.
    
    Returns *TRUE* iif the word was successfully added.
    
    """
        
    global word_info_flag

#    trace('sr_interface.addWord', 'adding \'%s\'' % word)
    
    #
    # First, fix the written form of the word
    #
# DCF: no, this is stupid.  Let the caller do this if desired, but allow
# the caller to add a word with the desired spoken form.

#    spoken, written = spoken_written_form(word)
#    word = vocabulary_entry(spoken, written, clean_written=1)


    #
    # Make sure we are connected to SR system
    #
                
    if word == 'g' or word == 'g\\g':
        raise RuntimeError('I thought we agreed not to add single letters')
    if getWordInfo(word) == None:
        trace('sr_interface.addWord', 'this word is new to NatSpeak')
                   
        if len(rest) == 0:
            flag = word_info_flag
        elif len(rest) == 1:
            flag = rest[0]
        else:
            return None
               
        try:
           natlink.addWord(word, flag)        
           sr_user_needs_saving = 1
        except:
           # In case the word's spoken form is rejected by
           # NatSpeak
           return None

        #
        # Note: Need to add redundant entry without special
        # characters (e.g. {Spacebar}}) in the written form,
        # because Select XYZ will not work if XYZ has some spaces
        # in its written form. This means that there will be two
        # vocabulary entries in the vocabulary. The entry without
        # spaces will always be used by Select XYZ, but
        # unfortunately, the dictation grammar may chose the one
        # without spaces over the one with spaces. Hopefully,
        # user correction will address that
        #
        word_no_special_chars = re.sub('{Spacebar}', '', word)
        if word_no_special_chars != word:
#            trace('sr_interface.addWord', 'adding redundant form with no spaces \'%s\'' % word_no_special_chars)
            try:
               natlink.addWord(word_no_special_chars, flag)
            except:
               return None
               
    return 1
 

def deleteWord(word, *rest):
    """Delete a word from NatSpeak's vocabulary.

    Also, we only remove it if the word was added to the vocabulary by
    VoiceCode, i.e. if the word info has 'added by Vocabulary Builder'
    flag set and if the word is a phrase (single words might actually
    have ben added by the real Vocabulary Builder)"""

#    trace('sr_interface.deleteWord', 'word=%s, rest=%s' % (word, rest))
    flag = getWordInfo(word)
    num_words = len(re.split('\s+', word))
    if addedByVC(flag) and num_words > 1:
#            trace('sr_interface.deleteWord', 'actually deleting word %s' % word)
        sr_user_needs_saving = 1
        return natlink.deleteWord(word)
    else:
#            trace('sr_interface.deleteWord', 'word not added by VoiceCode %s' % word)
        return None

def clean_written_form(written_form, clean_for=None):
    """Substitutes special charactes like newline and space in written form
    of a word.

    **INPUTS**

    *STR written_form* -- Written form to be cleaned.

    *STR clean_for* -- If 'sr', substitute the character to the form
     expected by the SR (e.g. ' ' -> {Spacebar}). If 'vc', substitute
     to the form expected by VoiceCode (e.g. '{Spacebar}' -> ' '). If
     neither of those, leave the form alone.

    **OUTPUT**

    *STR cleansed_form* -- The clean written form
    """

#    trace('sr_interface.clean_written_form', 'written_form=\'%s\', clean_for=%s' % (written_form, clean_for))
    cleansed_form = written_form
    if clean_for == 'sr': 
        cleansed_form = re.sub('\n', '{Enter}', cleansed_form)
        cleansed_form = re.sub('\s', '{Spacebar}', cleansed_form)
        cleansed_form = re.sub('\\\\', '{Backslash}', cleansed_form)
    elif clean_for == 'vc':
        cleansed_form = re.sub('\\{Enter\\}', '\n', cleansed_form)
        cleansed_form = re.sub('\\{Spacebar\\}', ' ', cleansed_form)
        cleansed_form = re.sub('\\{Backslash\\}', '\\\\', cleansed_form)        
#    trace('sr_interface.clean_written_form', 'cleansed_form=\'%s\'' % cleansed_form)
    return cleansed_form
    

def clean_spoken_form(spoken_form):
    """Cleans a spoken form.
    word.
    
    **INPUTS**
    
    *STR* spoken_form -- Spoken form to be cleansed
    
     **OUTPUTS**
    
    *STR* clean_form -- The cleansed form.
    """

#    trace('sr_interface.clean_spoken_form', 'spoken_form=\'%s\'' % spoken_form)

    clean_form = spoken_form

    # AD
    # Note: if spoken form is 'X.' where X is some capital letter, then leave
    # spoken form alone. Otherwise
    #   say(['O.'])
    # ends up calling
    #   recognitionMimic(['O.\\o'])
    #
    if not re.match(r'[A-Z]\.$', clean_form):

        #
        # Lower case
        #
        clean_form = string.lower(clean_form)

        #
        # Replace non alphanumericals by spaces.
        # 
        clean_form = re.sub('[^a-z0-9]+', ' ', clean_form)    
         
        #
        # Remove leading, trailing and multiple blanks
        #
        clean_form = re.sub('\s+', ' ', clean_form)
        clean_form = re.sub('^\s+', '', clean_form)
        clean_form = re.sub('\s+$', '', clean_form)

#    trace('sr_interface.clean_spoken_form', 'returning clean_form=\'%s\'' % clean_form)
    return clean_form

def spoken_written_form(vocabulary_entry, clean_written = 1, clean_spoken = 1):
    """Returns the written and spoken forms of a NatSpeak vocabulary entry
    
    **INPUTS**
    
    *STR* vocabulary_entry -- the vocabulary entry in either
    written or written\\spoken form.
    
    **OUTPUTS**
    
    *STR* (spoken, written) -- written and spoken forms of the vocabulary entry.
    """
    a_match = re.match('^([\s\S]*)\\\\([^\\\\]*)$', vocabulary_entry)
    if a_match:
        trace('sr_interface.spoken_written_form', 'entry \'%s\' is spoken/written form' % vocabulary_entry)
        
        #
        # Note: need to check for things like {Enter} in written_form
        # ignore for now
        #
        written = a_match.group(1)
# the try block handles the special case of selection grammar utterances which
# select words with different written and spoken form, which, in
# natspeak 7, get reported as written\spoken\t (That's backslash and t,
# not a tab character) - DCF
# But careful... if the word spoken was "backslash", then the written form is
# "\" (at least in NatSpeak 8 and 9). - AD
        spoken = a_match.group(2)
        trace('sr_interface.spoken_written_form', 
            'initial spoken, written = "%s", "%s"' % (spoken, written))
        extra = string.find(written, '\\')
        if extra >= 0 and written != "\\":
            was_written = written
            written = was_written[:extra]
            spoken = was_written[extra+1:]
            trace('sr_interface.spoken_written_form', 
                'but extra = %d' % extra)
            trace('sr_interface.spoken_written_form', 
                'final spoken, written = "%s", "%s"' % (spoken, written))
    else:
        trace('sr_interface.spoken_written_form', 'entry \'%s\' is just spoken ' % vocabulary_entry        )
        written = vocabulary_entry
        spoken = vocabulary_entry

    #
    # Substitute special characters in written form (e.g. ' ', '\n') to the
    # form that the SR expects (e.g. {Spacebar}, {Enter})
    #
    if clean_written:
        written = clean_written_form(written, clean_for='vc')

    #
    # Clean spoken form
    #
    if clean_spoken:
        spoken = clean_spoken_form(spoken)

#    trace('sr_interface.spoken_written_form', 'spoken=\'%s\', written=\'%s\'' % (spoken, written))

    return (spoken, written)
    

def vocabulary_entry(spoken_form, written_form = None, clean_written=1):
    """Creates a vocabulary entry with given spoken and written forms.

    **INPUTS**

    *STR* spoken_form -- the spoken form 

    *STR* *written_form -- the written form (default to *spoken_form*)
    
    *BOOL* clean_written -- If true, substitute special characters in
     written form (e.g. '\n' -> '{Enter}')

    **OUTPUTS**

    *entry* -- the entry to be added to the SR vocabulary
    """

#    trace('sr_interface.vocabulary_entry', 'spoken_form=\'%s\', written_form=%s, clean_written=%s' % (spoken_form, repr(written_form), clean_written))

# DCF: try removing this, to fix a problem with standard punctuation forms
#    spoken_form = clean_spoken_form(spoken_form)
# instead, just remove leading and trailing spaces
    spoken_form = string.strip(spoken_form)
    entry = spoken_form
    if not (written_form is None) and spoken_form != written_form:
        #
        # Substitute special characters in written form (e.g. {Spacebar},
        # {Enter}) to the form used in VoiceCode (e.g. ' ', '\n')
        #
        if clean_written:
            written_form = clean_written_form(written_form, clean_for='sr')

# DCF: ugh! no! words with empty written form should be allowed.  If you
# want identical written and spoken forms, omit written_form so it defaults 
# to None
#        if len(written_form) > 0:
        entry = written_form + '\\' + entry

#    trace('sr_interface.vocabulary_entry', 'returning entry=\'%s\'' % entry)
    return entry

def spoken_acronym(acronym):
    """returns the speech-engine specific spoken form for an acronym

    **INPUTS**

    *STR acronym* -- the acronym to expand.  The case of the
    original acronym is ignored

    **OUTPUTS**

    *STR* -- the string to use for the spoken form of the acronym
    """
# for right now, just use a Natspeak-specific implementation
    spoken = ''
    for letter in acronym:
        spoken = spoken + string.upper(letter) + '. '
    return spoken[:-1]
  

class SpokenUtteranceNL(SpokenUtterance.SpokenUtterance):
    """implementation of SpokenUtterance using natlink ResObj.
    Defines an interface for manipulating the speech
    information associated with a single user utterance

    **INSTANCE ATTRIBUTES**

    *ResObj* results -- natlink results object representing speech
    information
    *[(STR, STR)]* word_list -- list of spoken, written forms,
    either originally recognized or corrected with set_words
    
    *[STR]* spoken_only -- list of spoken forms,
    either originally recognized or corrected with set_words
    
    *[[(STR, STR))]* choices -- list of alternative choices retrieved
    from ResObj (ResObj may have more choices than this - see
    alternatives method)
    
    *INT* choices_available -- number of alternative choices available
    or -1 if unknown    

    *none*
    """
    def __init__(self, results, wave_playback = None, **attrs):
        """creates a SpokenUtteranceNL from a results object

        **INPUTS**

        *ResObj* results -- natlink results object representing speech
        information

        *CLASS* wave_playback -- class constructor for a concrete
        subclass of WavePlayback, or None if no playback is available

        **OUTPUTS**

        *none*
        """
        # local variables needed for initialization in deep_construct
        raw_words = results.getWords(0)
        trace('SpokenUtteranceNL.__init__', 
            "raw words = %s" % repr(raw_words))
        word_list = map(self.spoken_written_form, raw_words)
        trace('SpokenUtteranceNL.__init__', 
            "word list = %s" % repr(word_list))
        spoken_only = map(lambda x: x[0], word_list)
      
        self.deep_construct(SpokenUtteranceNL,
            {'results' : results, 
            'wave': None,
            'word_list': word_list,
            'spoken_only': spoken_only,
            'choices_available': -1,
            'choices': []}, 
            attrs)
        self.add_owned('wave')
        if wave_playback:
            try:
                wave_data = self.results.getWave()
                wave = wave_playback(data = wave_data)
#            print 'utterance created wave -- checking'
                if wave.check():
#                print 'utterance: wave ok'
                    self.wave = wave
            except DataMissing:
#            print 'data missing'
                pass

    def spoken_forms(self):
        """returns list of spoken forms from the utterance

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of spoken forms from the utterance
        """
        return self.spoken_only

    def words(self):
        """returns list of words (as (spoken, written) 2-tuples) 
        from the utterance.

        **INPUTS**

        *none*

        **OUTPUTS**

        *[(STR, STR)]* -- list of words (as (spoken, written) 2-tuples) 
        
        """
        return self.word_list

    def entry(self, spoken, written = None):
        """returns a vocabulary entry in the form used by
        NaturallySpeaking/natlink.  If the written form is omitted, then it is
        assumed to be identical to the spoken form.

        **INPUTS**

        *STR* spoken -- spoken form
        *STR* written -- written form (if different)

        **OUTPUTS**

        *STR* -- vocabulary entry in written\spoken form as used by
        NaturallySpeaking
        """
        if  written == None:
            written = spoken
        return vocabulary_entry(spoken, written) 

    def entry_tuple(self, word):
        """returns a vocabulary entry in the form used by
        NaturallySpeaking/natlink.  

        **INPUTS**

        *(STR, STR)* word -- word (as (spoken, written) 2-tuples) 

        **OUTPUTS**

        *STR* -- vocabulary entry in written\spoken form as used by
        NaturallySpeaking
        """
        return vocabulary_entry(word[0], word[1])

    def same_written(self, spoken_forms):
        """converts a list of spoken forms to a list of (spoken, written) 
        2-tuples.

        **INPUTS**

        *[STR]* spoken_forms -- list of spoken forms 
        (written forms will be assumed to be identical)
        
        **OUTPUTS**

        *[(STR, STR)]* -- list of words (as (spoken, written) 2-tuples) 
        """
        return map(lambda s: (s, s), spoken_forms)

    def entries_spoken( self, spoken_forms):
        """converts a list of spoken forms to a list of vocabulary entries
        in the form used by NaturallySpeaking/natlink.

        **INPUTS**

        *[STR]* spoken_forms -- list of spoken forms 
        (written forms will be assumed to be identical)

        **OUTPUTS**

        *[STR]* -- list of vocabulary entries in written\spoken form as used by
        NaturallySpeaking
        """
        return map(self.entry, spoken_forms)

    def spoken_written_form(self, entry):
        """converts a vocabulary entry in the form used by 
        NaturallySpeaking/natlink to a 2-tuple of spoken, written.

        **INPUTS**

        *STR* -- vocabulary entry in written\spoken form as used by
        NaturallySpeaking

        **OUTPUTS**

        *(STR, STR)* word -- word (as (spoken, written) 2-tuples) 
        """
        return spoken_written_form(entry, clean_spoken = 0)

    def adapt(self, words):
        """changes the stored list of words so that 
        subsequent correction boxes can display the corrected list, and
        informs the speech engine of the corrected list of words, so
        it can adapt.

        **INPUTS**

        *[(STR, STR)]* -- corrected list of words 
        (as (spoken, written) 2-tuples) 

        **OUTPUTS**

        *BOOL* -- true if the adaption was accepted
        """
        list = []
        for spoken, written in words:
            list.append(vocabulary_entry(spoken, written, clean_written = 0))
        success = 0
        try:
            success = self.results.correction(list)
        except natlink.InvalidWord:
# if the ResObj raises an InvalidWord exception, do not set the 
#         word list
            return 0
        self.set_words(words)
        return success

    def adapt_spoken(self, spoken_forms):
        """changes the stored list of words so that 
        subsequent correction boxes can display the corrected list, and
        informs the speech engine of the corrected list of words, so
        it can adapt.


        **INPUTS**

        *[STR]* spoken_forms -- corrected list of spoken forms 
        (written forms will be assumed to be identical)

        **OUTPUTS**

        *BOOL* -- true if the adaption was accepted
        """
        words = self.same_written(spoken_forms)
        return self.adapt(words)

    def set_words(self, words):
        """changes the stored list of words (after correction) so that 
        subsequent correction boxes can display the corrected list.
        The results object is unaffected.

        **INPUTS**

        *[(STR, STR)]* -- corrected list of words 
        (as (spoken, written) 2-tuples) 

        **OUTPUTS**

        *none*

        """
        self.word_list = words
        self.spoken_only = map(lambda x: x[0], words)

    def set_spoken(self, spoken_forms):
        """changes the stored list of words (after correction) so that 
        subsequent correction boxes can display the corrected list.
        The results object is unaffected.

        **INPUTS**

        *[STR]* spoken_forms -- corrected list of spoken forms 
        (written forms will be assumed to be identical)

        **OUTPUTS**

        *none*

        """
        words = self.same_written(spoken_forms)
        self.set_words(words)

    def playback_available(self):
        """indicates whether playback of the utterance is available.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if playback is available, false if it is not
        (because utterance wasn't actually spoken, or speech data has 
        been lost, or because the implementation doesn't support
        playback)
        """
        if self.wave:
            return 1
        return 0

    def playback(self):
        """plays back recorded utterance.

        Playback is synchronous.  It will handle turning the microphone
        off and back on again (if necessary)

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true unless playback was unavailable, or 
        there was an error in playback (e.g.  another program had 
        control of the audio device)
        """
        if self.wave and self.wave.check():
            mic_was = get_mic()
            set_mic('off')
            self.wave.play()
            set_mic(mic_was)
            return 1
        return 0
      
    def can_be_adapted(self):
        """indicates whether the utterance can be corrected for adaption
        of the speech engine.  Utterances for which there was no speech
        information or for which the speech information has been lost or
        discarded may not be adaptable.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the speech information is available for adaption.
        """
        return 1
      
    def alternatives_available(self):
        """returns number of recognition alternatives available 
        (for the whole utterance), or -1 if the number
        is unknown.

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- number of recognition alternatives available (including
        the original), or -1 if the number is unknown (NaturallySpeaking
        doesn't indicate the total number, but simply lets you keep
        asking for the next one until it runs out.
        """
        return self.choices_available

    def _get_alternative(self, choice):
        """private method for getting a single alternative and converting it to
        list of 2-tuples.
        **INPUTS**

        *INT* choice -- number of the choice to request

        **OUTPUTS**

        *[(STR, STR)]* -- list of words (as (spoken, written) 
        2-tuples) corresponding to that alternative, or None if choice
        was unavailable
        """
        try:
            list = self.results.getWords(choice)
        except natlink.OutOfRange:
            return None
        return map(self.spoken_written_form, list)

    def alternatives(self, n):
        """returns the best recognition alternatives available 
        (for the whole utterance) including the original.  
        Will not return more than n alternatives, but may return fewer
        (if the speech engine has not provided that many).

        Note: the first alternative in the list will not be identical
        to the output of words(), if the phrase has previously been
        corrected with set_words.

        **INPUTS**

        *INT* n -- number of alternatives requested

        **OUTPUTS**

        *[[(STR, STR)]]* -- list of list of words (as (spoken, written) 
        2-tuples) 
        
        """
        if n <= len(self.choices):
            return self.choices[0:n-1]
        if self.choices_available >= 0:
            return self.choices
# if number of choices available is known, then we already have all
# available choices, so just return what we have.
# otherwise, try to get more choices
        for choice in range(len(self.choices), n):
            words = self._get_alternative(choice)
            if words == None:
# no more available, so now we know how many are available
                self.choices_available = choice
                break
            self.choices.append(words)
# return what we have.
        return self.choices
        

           
        


