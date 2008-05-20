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

"""Classes for dealing with programming symbols and their spoken forms.
"""


from Object import Object, OwnerObject
import vc_globals
import util
import debug

import copy, exceptions, os, re, string, sys
import traceback


class SymElement(Object):
    """abstract base class for elements (words, aliases, explicit
    capitalization macros) which can be added to a symbol
    builder to form symbols

    This class defines an interface for double dispatch (see GoF visitor
    pattern, for example, or Stroustrup streams chapter), with the
    method called depending on the subclass of SymElement and on that of
    the SymBuilder
    """
    def __init__(self, **args):
        self.deep_construct(SymElement, {}, args)

    def add_to(self, builder):
        """add self to the symbol builder, using the appropriate method
        """
        debug.virtual('SymElement.add_to')

class SymBuilder(Object):
    """abstract base class for objects which progressively build up
    symbol names word by word
    """
    def __init__(self, **args):
        self.deep_construct(SymBuilder, {'spoken': []}, args)
        
    def build_from_words(self, words):
        """build a symbol from a complete list of words.
        
        **INPUTS**
        
        *[STR] words* -- the list of words
        
        **OUTPUTS**
        
        *STR* -- the symbol
        """
        for a_word in words:
           self.add_word(a_word)
        return self.finish()

    def add_word(self, word, original = None):
        """appends a new word to the symbol
        
        **INPUTS**
        
        *STR word* -- the new word

        *STR original* -- original, unabbreviated version (if word is an
        abbreviation/substitution) or None if word is not abbreviated
        
        **OUTPUTS**
        
        *none*
        """
        if original:
            self.spoken.append(original)
        else:
            self.spoken.append(word)
        
    def add_letter(self, letter, spoken):
        """appends a single letter to the symbol
        
        **INPUTS**
        
        *STR letter* -- the new letter
        
        *STR spoken* -- spoken form of the letter

        **OUTPUTS**
        
        *none*
        """
        self.spoken.append(spoken)

    def finish(self):
        """finish building the symbol (allows for SymBuilder subclasses
        with fixed suffixes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the final symbol
        """
        debug.virtual('SymBuilder.finish')

    def empty(self):
        """is the symbol currently empty (i.e. invisible elements like
        change_capitalization, suppress_separator, or
        suppress_abbreviation have been processed so far, and there is
        no fixed prefix or suffix for the symbol)
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the symbol is empty
        """
        debug.virtual('SymBuilder.empty')


    def change_caps(self, caps, one_word = 1):
        """manually adjust capitalization of the following word or words
        in the symbol
        
        **INPUTS**
        
        *STR caps* -- the new capitalization state: 'no-caps', 'normal', 
        'cap', or 'all-caps'

        *BOOL one_word* -- if true, modify capitalization for the next
        word.  If false, modify for all following words until a
        subsequent change_caps with one_word = 0.  (A subsequent call to
        change_caps with one_word = 1 will take precedence temporarily)
        
        **OUTPUTS**
        
        *none*
        """
        debug.virtual('SymBuilder.change_caps')

    def capitalize(self, word, caps):
        """capitalizes a word according to the capitalization state flag
        
        **INPUTS**
        
        *STR word* -- the word 

        *STR caps* -- the new capitalization state: 'no-caps', 
        'cap', or 'all-caps' (note: this function does not handle the
        'normal' value, because that is intended to restore the default
        capitalization for the particular concrete subclass of
        SymBuilder, which SymBuilder does not know.

        **OUTPUTS**

        *STR* -- the word, formatted as specified
        """
        if caps == 'no-caps':
            return string.lower(word)
        if caps == 'cap':
            return string.capitalize(word)
        if caps == 'all-caps':
            return string.upper(word)
        return word

    def suppress_separator(self):
        """manually suppress separator before the next word of the
        symbol
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *none*
        """ 
        debug.virtual('SymBuilder.suppress_separator')

    def suppress_abbreviation(self):
        """manually suppress abbreviation of the next word of the
        symbol
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *none*
        """ 
        debug.virtual('SymBuilder.suppress_abbreviation')

    def spoken_form(self):
        """returns the spoken form of the new symbol

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        
        debug.trace('SymBuilder.spoken_form', 
            'joining %s' % repr(self.spoken))
        return string.join(self.spoken)

class BuilderRegistry(Object):
    """maintains a registry mapping names of concrete subclasses of
    SymBuilder to parameter-free callable objects which construct them

    **INSTANCE ATTRIBUTES**

    *{STR: FCT()} builders* -- dictionary mapping names of concrete 
    subclasses of SymBuilder to parameter-free callable objects 
    which construct them.  The values of the dictionary must be callable
    without any arguments and must return concrete SymBuilder instances.
    Typically, the values will be class objects, but this is not
    required.

    *[STR] order* -- list of names of concrete 
    subclasses of SymBuilder in the order in which they were registered.
    If SymBuilderFactory is not configured with any preferences, this
    order will be used as a fallback.
    """
    def __init__(self, **args):
        self.deep_construct(BuilderRegistry, 
                            {
                             'builders': {},
                             'order': []
                            }, args)

    def register(self, name, constructor):
        """adds a named SymBuilder to the registry

        **INPUTS** 

        *STR name* -- unique name for the SymBuilder.  To avoid
        conflicts with user-defined names, standard SymBuilders should
        start with the prefix 'std_' and user-defined names should never
        start with this prefix.

        *FCT() constructor* -- A class object or other callable object
        returning an instance of  a concrete subclass of SymBuilder.
        Constructor must be callable without any arguments.

        **OUTPUTS**

        *BOOL* -- true if the constructor was registered, but 
        false if it was not because the name was already taken
        """
        if self.builders.has_key(name):
            return 0
        test = constructor()
        if not isinstance(test, SymBuilder):
            return 0
        self.builders[name] = constructor
        self.order.append(name)
        return 1

    def make_builder(self, name):
        """create a new instance of the concrete SymBuilder subclass
        corresponding to the given name

        **INPUTS**

        *STR name* -- the name given when the particular type of
        SymBuilder was registered

        **OUTPUTS**

        *SymBuilder* -- the SymBuilder instance, or None if the name was
        unknown
        """
        try:
            constructor = self.builders[name]
        except KeyError:
            return None
        return constructor()

    def known_builder(self, name):
        """indicates whether there is a builder with the given name

        **INPUTS**

        *STR name* -- the name given when the particular type of
        SymBuilder was registered

        **OUTPUTS**

        *BOOL* -- true if the named builder is registered
        """
        return self.builders.has_key(name)

    def default_order(self):
        """returns a default order for the registered builders, to use
        as a fallback if the user doesn't specify his or her own
        preferences
        """
        return copy.copy(self.order)

# module-global registry
registry = BuilderRegistry()

class SymBuilderFactoryState(Object):
    """
    Data used to store and restore the state of SymBuilderFactory
    """
    def __init__(self, next_builder, expected_type, **args):
        self.deep_construct(SymBuilderFactoryState,
                            {
                             'next_builder': next_builder,
                             'expected_type': expected_type
                            },
                            args)
        
class SymBuilderFactory(Object):
    """object factory which manages explicit requests for particular
    SymBuilder classes as well as preferences based on the type of
    identifier expected

    *{STR: STR} identifier_types* -- map from identifier types to their
    parent types

    *BuilderRegistry builders* -- factory which constructs SymBuilder
    objects by name

    *STR next_builder* -- the preference for the next SymBuilder, if
    the previous CSC specified one, or None

    *STR expected_type* -- the type of identifier expected next

    *{STR: {STR: [STR]}} preferences* -- map from language names to map
    from identifier types to lists of preferred builders
    """
    def __init__(self, **args):
        self.deep_construct(SymBuilderFactory, 
                            {
                             'next_builder': None,
                             'expected_type': None,
                             'identifier_types': {},
                             'preferences': {},
                             'builders': registry,
                            },
                            args)

    def add_identifier(self, identifier, parent = None):
        """defines a new identifier type

        *STR identifier* -- name of the new identifier type (must NOT be
        a known identifier type, or a RuntimeError will be raised)

        *STR parent* -- name of the parent (must be a known identifier
        type, or None, or a RuntimeError will be raised)

        **OUTPUTS**
        """
        if not (parent is None):
            if not self.identifier_types.has_key(parent):
                raise RuntimeError('parent is unknown identifier type %s' \
                    % parent)
        if self.identifier_types.has_key(identifier):
            raise RuntimeError('duplicate identifier type %s' % identifier)
        self.identifier_types[identifier] = parent

    def verify_identifier(self, identifier):
        """verifies whether an identifier type with a given name is 
        known to the SymBuilderFactory

        **INPUTS**

        *STR identifier* -- name of the identifier type

        **OUTPUTS**

        *BOOL* -- true if the identifier type is known
        """
        if identifier is None:
            return 1
        return self.identifier_types.has_key(identifier)

    def verify_builder(self, builder):
        """verifies whether a builder with a given name is known to the
        SymBuilderFactory

        **INPUTS**

        *STR builder* -- name of the builder

        **OUTPUTS**

        *BOOL* -- true if the builder name is known
        """
        return self.builders.known_builder(builder)

    def set_preferences(self, builders, identifier = None, language = None):
        """establishes the preferred order for symbol formatting styles
        for a given language and identifier type

        **INPUTS**

        *[STR] builders* -- prioritized list of names of registered SymBuilder 
        objects. If one of the builders is unknown, set_preferences raises 
        a RuntimeError.

        *STR identifier* -- name of the identifier to which these
        preference apply, or None to set general preferences for all
        identifiers without their own preferences.  If the identifier type 
        is unknown, set_preferences raises a RuntimeError.

        *STR language* -- name of the language to which these
        preference apply, or None to set general preferences for all
        languages

        **OUTPUTS**

        *none*
        """
        try:
            preferences = self.preferences[language]
        except KeyError:
            preferences = {}
            self.preferences[language] = preferences
        if not (identifier is None):
            if not self.identifier_types.has_key(identifier):
                raise RuntimeError('unknown identifier type %s' % identifier)
        for builder in builders:
            if not self.builders.known_builder(builder):
                raise RuntimeError('unknown builder %s' % builder)
        preferences[identifier] = copy.copy(builders)

    def expect(self, identifier):
        """method used to tell the SymBuilderFactory to expect a
        particular type of identifier

        **INPUTS**

        *STR identifier* -- name of the identifier type, or None for a
        generic identifier

        **OUTPUTS**

        *BOOL* -- true if the identifier is known
        """
        if self.verify_identifier(identifier):
            self.expected_type = identifier
        else:
            self.expected_type = None

    def prefer(self, builder):
        """method used to tell the SymBuilderFactory that a CSC has
        given an explicit preference for particular SymBuilder to be
        used for the next symbol

        **INPUTS**

        *STR identifier* -- name of the identifier type, or None for a
        generic identifier

        **OUTPUTS**

        *BOOL* -- true if the identifier is known
        """
        if self.verify_builder(builder):
            self.next_builder = builder
        else:
            self.next_builder = None

    def get_state(self):
        """
        Returns the data needed to restore the factory to its present
        state.  The caller should not rely on any particular structure
        of the data, and should only use it by passing it back to
        restore_state.

        ** INPUTS **

        *none*

        ** OUTPUTS **

        *SymBuilderFactoryState* -- the state, which can be passed
        back to restore_state
        """
        debug.trace('SymBuilderFactory.get_state',
                    'next_builder = %s, expected_type = %s' \
                    % (self.next_builder, self.expected_type))
        return SymBuilderFactoryState(self.next_builder,
                                      self.expected_type)

    def restore_state(self, state):
        """
        Restores SymBuilderFactory to the previously stored state.  If
        the previous state cannot be restored, then the state is
        cleared.

        ** INPUTS **

        *SymBuilderFactoryState state* -- the state data previously
        returned by get_state

        ** OUTPUTS **

        *BOOL* -- true if the state was successfully restored
        (otherwise the state is cleared)
        """
        debug.trace('SymBuilderFactory.restore_state',
                    'next_builder = %s, expected_type = %s' \
                    % (state.next_builder, state.expected_type))
        self.prefer(state.next_builder)
        self.expect(state.expected_type)
   
    def clear(self):
        """clear expectations and preferences for the next identifier

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.next_builder = None
        self.expected_type = None
        
    def manually_specified(self):
        """Indicates whether the style of the following symbol has been
        manually specified

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the style has been manually specified
        """
        return not not self.next_builder

    def current_preferences(self, buff):
        """return the list of preferences for a new SymBuilder, given
        the current SymBuilderFactory state, and the language and
        project of the current buffer

        **INPUTS**

        *SourceBuff buff* -- the SourceBuff object corresponding to the
        current buffer (used to check the current language and project)

        **OUTPUTS**

        *[STR]* -- list of names of preferred builders
        """
# project settings will take precedence, when they are implemented
        language = buff.language_name()
        debug.trace('SymBuilderFactory.current_preferences', 
            'identifier %s, language %s' % (self.expected_type, language))
        debug.trace('SymBuilderFactory.current_preferences', 
            'try language-specific')
        preferences = self.by_language(self.expected_type, language)
        if not preferences:
            debug.trace('SymBuilderFactory.current_preferences', 
                'try any language')
            preferences = self.by_language(self.expected_type, None)
            if not preferences:
                debug.trace('SymBuilderFactory.current_preferences', 
                    'using default order')
                preferences = self.builders.default_order()
        debug.trace('SymBuilderFactory.current_preferences', 
            'preferences: %s' % preferences)
        if self.next_builder:
            debug.trace('SymBuilderFactory.current_preferences', 
                'prepending explicit preference')
            try:
                preferences.remove(self.next_builder)
            except ValueError:
                pass
            preferences.insert(0, self.next_builder)
            debug.trace('SymBuilderFactory.current_preferences', 
                'preferences: %s' % preferences)
        return preferences

    def by_language(self, identifier, language):
        """Look up the preferences for a given identifier type in the
        language-specific map 

        **INPUTS**

        *STR identifier* -- name of the identifier type

        *STR language* -- language

        **OUTPUTS**

        *[STR]* -- list of names of preferred builders, or None if the
        language is unknown
        """
        debug.trace('SymBuilderFactory.by_language', 
            'identifier %s, language %s' % (identifier, language))
        if not self.verify_identifier(identifier):
            debug.trace('SymBuilderFactory.by_language', 
                'unknown identifier %s' % identifier)
            return self.builders.default_order()
        try:
            language_map = self.preferences[language]
        except KeyError:
            debug.trace('SymBuilderFactory.by_language', 
                'no map for language %s' % language)
            return None
        while 1:
            try:
                return copy.copy(language_map[identifier])
            except KeyError:
                debug.trace('SymBuilderFactory.by_language', 
                    'no entry for identifier %s' % identifier)
                pass
            if identifier is None:
                debug.trace('SymBuilderFactory.by_language', 
                    'returning default order')
                return self.builders.default_order()
# if no preferences for that identifier, and identifier is not None,
# then try parent type
            try:
                # try parent type
                identifier = copy.copy(self.identifier_types[identifier])
                debug.trace('SymBuilderFactory.by_language', 
                    'trying parent: %s' % identifier)
            except KeyError:
# if no parent type, then try generic preferences
                debug.trace('SymBuilderFactory.by_language', 
                    'no parent type, try None')
                identifier = None
                return None

    def new_builder(self, buff):
        """create a new SymBuilder of the appropriate type

        **INPUTS**

        *SourceBuff buff* -- the SourceBuff object corresponding to the
        current buffer (used to check the current language and project)

        **OUTPUTS**

        *SymBuilder* -- the new symbol builder
        """
        builder = self.next_builder
        if not builder:
            preferences = self.current_preferences(buff)
            builder = preferences[0]
        return self.make_builder(builder)

    def make_builder(self, name):
        """create a new instance of the concrete SymBuilder subclass
        corresponding to the given name

        **INPUTS**

        *STR name* -- the name given when the particular type of
        SymBuilder was registered

        **OUTPUTS**

        *SymBuilder* -- the SymBuilder instance, or None if the name was
        unknown
        """
        return self.builders.make_builder(name)

class ManualCaps(Object):
    """mix-in class for SymBuilder which maintains an
    internal capitalization state taking into account manual
    overriding of capitalization preferences

    NOTE: subclasses of ManualCaps must use capitalization_state to
    check the current state once (and only once) per add_word call

    NOTE: ManualCaps is a mix-in which overrides the change_caps method
    of SymBuilder, so subclasses must inherit from ManualCaps before
    inheriting from SymBuilder
    """
    def __init__(self, **args):
        self.deep_construct(ManualCaps, 
                            {
                             'ongoing_caps': 'normal',
                             'current_caps': 'normal',
                             'one_word': 0
                            },
                            args)

    def capitalization_state(self):
        """check the current capitalization state, and reset it to the
        default if it was temporary

        NOTE: subclasses of ManualCaps must use capitalization_state to
        check the current state once (and only once) per add_word call
        
        **INPUTS**

        *none*
        
        **OUTPUTS**

        *STR* -- the capitalization state for the next word: 'no-caps', 
        'cap', 'normal', or 'all-caps'
        """
        state = self.current_caps
        if self.one_word:
            self.current_caps = self.ongoing_caps
        debug.trace('ManualCaps.capitalization_state', 
            'state = %s' % repr(state))
        return state

    def change_caps(self, caps = None, one_word = 1):
        """manually adjust capitalization of the following word or words
        in the symbol
        
        **INPUTS**
        
        *STR caps* -- the new capitalization state: 'no-caps', 'normal', 
        'cap', or 'all-caps'

        *BOOL one_word* -- if true, modify capitalization for the next
        word.  If false, modify for all following words until a
        subsequent change_caps with one_word = 0.  (A subsequent call to
        change_caps with one_word = 1 will take precedence temporarily)
        
        **OUTPUTS**
        
        *none*
        """
        debug.trace('ManualCaps.change_caps', 
            'caps, one_word = %s, %d' % (caps, one_word))
        self.current_caps = caps
        if one_word:
            self.one_word = 1
        else:
            self.ongoing_caps = caps
            self.one_word = 0

class ManualSuppression(Object):
    """partially concrete subclass of SymBuilder which maintains
    internal states taking into account manual suppression of
    abbreviation and separators.

    NOTE: subclasses of ManualSuppression must use both separator_state
    and abbreviation_state to check the current states once (and only once) 
    per add_word call

    NOTE: ManualSuppression is a mix-in which overrides the change_caps method
    of SymBuilder, so subclasses must inherit from ManualSuppression before
    inheriting from SymBuilder
    """
    def __init__(self, **args):
        self.deep_construct(ManualSuppression, 
                            {
                             'use_sep': 1,
                             'use_abbrev': 1
                            }, args)

    def suppress_separator(self):
        """manually suppress separator before the next word of the
        symbol
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *none*
        """ 
        self.use_sep = 0

    def suppress_abbreviation(self):
        """manually suppress abbreviation of the next word of the
        symbol
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *none*
        """ 
        self.use_abbrev = 0

    def separator_state(self):
        """check the current separator state, and reset it to true
        
        NOTE: subclasses of ManualSuppression must use both separator_state
        and abbreviation_state to check the current states once (and only once) 
        per add_word call

        **INPUTS**

        *none*
        
        **OUTPUTS**

        *BOOL* -- true if a separator (if any) should be used before the next
        word 
        """
        state = self.use_sep
        self.use_sep = 1
        return state

    def abbreviation_state(self):
        """check the current abbreviation state, and reset it to the
        default 
        
        NOTE: subclasses of ManualSuppression must use both separator_state
        and abbreviation_state to check the current states once (and only once) 
        per add_word call

        **INPUTS**

        *none*
        
        **OUTPUTS**

        *BOOL* -- true if the next word should be abbreviated if
        possible
        """
        state = self.use_abbrev
        self.use_abbrev = 1
        return state

class FixedCaps(ManualCaps):
    """subclass of ManualCaps for symbol builders which use a 
    fixed default capitalization state 

    NOTE: subclasses of FixedCaps must use capitalization_state to
    check the current state once (and only once) per add_word call

    NOTE: FixedCaps is a mix-in which overrides the change_caps method
    of SymBuilder, so subclasses must inherit from FixedCaps before
    inheriting from SymBuilder
    """
    def __init__(self, default_caps = None, **args):
        self.deep_construct(FixedCaps, 
                            {
                             'default_caps': default_caps
                            },
                            args)
        if default_caps is None:
            self.default_caps = 'normal'

    def capitalization_state(self):
        """check the current capitalization state, and reset it to the
        default if it was temporary
        
        **INPUTS**

        *none*
        
        **OUTPUTS**

        *STR* -- the capitalization state for the next word: 'no-caps', 
        'cap', 'normal', or 'all-caps'
        """
        state = ManualCaps.capitalization_state(self)
        if state == 'normal':
            state = self.default_caps
        debug.trace('FixedCaps.capitalization_state', 
            'effective state = %s' % repr(state))
        return state

class BuildInterCaps(FixedCaps, ManualSuppression, SymBuilder):
    """builds symbols in InterCaps (all words capitalized, no other
    separators
    """
    def __init__(self, **args):
        self.deep_construct(BuildInterCaps, 
                            {'symbol': ""}, args, 
                            enforce_value = {'default_caps': 'cap'})

    def add_letter(self, letter, spoken):
        """appends a single letter to the symbol
        
        **INPUTS**
        
        *STR letter* -- the new letter
        
        *STR spoken* -- spoken form of the letter

        **OUTPUTS**
        
        *none*
        """
        self.add_word(letter, original = spoken)

    def add_word(self, word, original = None):
        """appends a new word to the symbol
        
        **INPUTS**
        
        *STR word* -- the new word

        *STR original* -- original, unabbreviated version (if word is an
        abbreviation/substitution) or None if word is not abbreviated
        
        **OUTPUTS**
        
        *none*
        """
        debug.trace('BuildInterCaps.add_word',
            'default_caps = %s' % repr(self.default_caps))
        SymBuilder.add_word(self, word, original)
        if original and not self.abbreviation_state():
            word = original
        self.separator_state() 
        # no separators, but still need to check this once per add_word
        debug.trace('BuildInterCaps.add_word',
            'word before capitalize = %s' % repr(word))
        debug.trace('BuildInterCaps.add_word',
            'default_caps = %s' % repr(self.default_caps))
        word = self.capitalize(word, self.capitalization_state())
        debug.trace('BuildInterCaps.add_word',
            'word after capitalize = %s' % repr(word))
        debug.trace('BuildInterCaps.add_word',
            'default_caps = %s' % repr(self.default_caps))
        self.symbol = self.symbol + word

    def finish(self):
        """finish building the symbol (allows for SymBuilder subclasses
        with fixed suffixes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the final symbol
        """
        return self.symbol

    def empty(self):
        """is the symbol currently empty (i.e. invisible elements like
        change_capitalization, suppress_separator, or
        suppress_abbreviation have been processed so far, and there is
        no fixed prefix or suffix for the symbol)
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the symbol is empty
        """
        return self.symbol == ""

registry.register('std_intercaps', BuildInterCaps)

class BuildLowerInterCaps(FixedCaps, ManualSuppression, SymBuilder):
    """builds symbols in lowerInterCaps (all words except the first 
    capitalized, no other separators
    """
    def __init__(self, **args):
        self.deep_construct(BuildLowerInterCaps, 
                            {'symbol': ""}, args, 
                            enforce_value = {'default_caps': 'cap'})
        self.change_caps('no-caps')

    def add_letter(self, letter, spoken):
        """appends a single letter to the symbol
        
        **INPUTS**
        
        *STR letter* -- the new letter
        
        *STR spoken* -- spoken form of the letter

        **OUTPUTS**
        
        *none*
        """
        self.add_word(letter, original = spoken)

    def add_word(self, word, original = None):
        """appends a new word to the symbol
        
        **INPUTS**
        
        *STR word* -- the new word

        *STR original* -- original, unabbreviated version (if word is an
        abbreviation/substitution) or None if word is not abbreviated
        
        **OUTPUTS**
        
        *none*
        """
        SymBuilder.add_word(self, word, original)
        if original and not self.abbreviation_state():
            word = original
        self.separator_state() 
        # no separators, but still need to check this once per add_word
        word = self.capitalize(word, self.capitalization_state())
        self.symbol = self.symbol + word

    def finish(self):
        """finish building the symbol (allows for SymBuilder subclasses
        with fixed suffixes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the final symbol
        """
        return self.symbol

    def empty(self):
        """is the symbol currently empty (i.e. invisible elements like
        change_capitalization, suppress_separator, or
        suppress_abbreviation have been processed so far, and there is
        no fixed prefix or suffix for the symbol)
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the symbol is empty
        """
        return self.symbol == ""

registry.register('std_lower_intercaps', BuildLowerInterCaps)

class BuildRunTogether(FixedCaps, ManualSuppression, SymBuilder):
    """builds symbols with words run together and no capitalization to
    separate them (lousy style which no one should ever use, but some
    libraries do, so we need to be able to dictate it)
    """
    def __init__(self, **args):
        self.deep_construct(BuildRunTogether, 
                            {'symbol': ""}, args)

    def add_letter(self, letter, spoken):
        """appends a single letter to the symbol
        
        **INPUTS**
        
        *STR letter* -- the new letter
        
        *STR spoken* -- spoken form of the letter

        **OUTPUTS**
        
        *none*
        """
        self.add_word(letter, original = spoken)

    def add_word(self, word, original = None):
        """appends a new word to the symbol
        
        **INPUTS**
        
        *STR word* -- the new word

        *STR original* -- original, unabbreviated version (if word is an
        abbreviation/substitution) or None if word is not abbreviated
        
        **OUTPUTS**
        
        *none*
        """
        SymBuilder.add_word(self, word, original)
        if original and not self.abbreviation_state():
            word = original
        self.separator_state() 
        # no separators, but still need to check this once per add_word
        word = self.capitalize(word, self.capitalization_state())
        self.symbol = self.symbol + word

    def finish(self):
        """finish building the symbol (allows for SymBuilder subclasses
        with fixed suffixes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the final symbol
        """
        return self.symbol

    def empty(self):
        """is the symbol currently empty (i.e. invisible elements like
        change_capitalization, suppress_separator, or
        suppress_abbreviation have been processed so far, and there is
        no fixed prefix or suffix for the symbol)
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the symbol is empty
        """
        return self.symbol == ""

registry.register('std_run_together', BuildRunTogether)

class BuildUnder(FixedCaps, ManualSuppression, SymBuilder):
    """builds symbols with words separated by underscores (but with
    underscores suppressed when one of the adjacent characters is a
    digit), and default capitalization 
    """
    def __init__(self, **args):
        self.deep_construct(BuildUnder, 
                            {'symbol': "", 'single': 0}, args)

    def add_word(self, word, original = None):
        """appends a new word to the symbol
        
        **INPUTS**
        
        *STR word* -- the new word

        *STR original* -- original, unabbreviated version (if word is an
        abbreviation/substitution) or None if word is not abbreviated
        
        **OUTPUTS**
        
        *none*
        """
        SymBuilder.add_word(self, word, original)
        debug.trace('BuildUnder.add_word', 'word = %s' % word)
        debug.trace('BuildUnder.add_word', 'original = %s' % original)
        self.single = 0
        if original and not self.abbreviation_state():
            word = original
        state = self.separator_state() 
        word = self.capitalize(word, self.capitalization_state())
        debug.trace('BuildUnder.add_word', 'now, word = %s' % word)
        debug.trace('BuildUnder.add_word', 'symbol = %s' % repr(self.symbol))
        if not word:
            return
        if state and self.symbol:
            last_char = self.symbol[-1]
            first_char = word[0]
            if not (last_char.isdigit() or first_char.isdigit() \
                    or last_char == '_' or first_char == '_'):
                self.symbol = self.symbol + '_'
        self.symbol = self.symbol + word
        debug.trace('BuildUnder.add_word', 'symbol = %s' % repr(self.symbol))

    def add_letter(self, letter, spoken):
        """appends a single letter to the symbol
        
        **INPUTS**
        
        *STR letter* -- the new letter
        
        *STR spoken* -- spoken form of the letter


        **OUTPUTS**
        
        *none*
        """
        SymBuilder.add_letter(self, letter, spoken)
        debug.trace('BuildUnder.add_letter', 
            'letter = %s, symbol = %s' % (letter, repr(self.symbol)))
        if self.single:
            self.abbreviation_state()
            state = self.separator_state() 
            letter = self.capitalize(letter, self.capitalization_state())
            self.symbol = self.symbol + letter
            debug.trace('BuildUnder.add_letter', 
                'letter = %s, symbol = %s' % (letter, repr(self.symbol)))
        else:
            self.add_word(letter)
        self.single = 1

    def finish(self):
        """finish building the symbol (allows for SymBuilder subclasses
        with fixed suffixes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the final symbol
        """
        return self.symbol

    def empty(self):
        """is the symbol currently empty (i.e. invisible elements like
        change_capitalization, suppress_separator, or
        suppress_abbreviation have been processed so far, and there is
        no fixed prefix or suffix for the symbol)
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the symbol is empty
        """
        return self.symbol == ""

registry.register('std_underscores', BuildUnder)

class BuildUpperUnder(BuildUnder):
    """builds symbols with all-caps words separated by underscores (but with
    underscores suppressed when one of the adjacent characters is a
    digit)
    """
    def __init__(self, **args):
        self.deep_construct(BuildUpperUnder, {}, args, 
                            enforce_value = {'default_caps': 'all-caps'})

registry.register('std_all_caps_underscores', BuildUpperUnder)
