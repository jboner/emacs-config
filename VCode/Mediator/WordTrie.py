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
# (C)2003, David C. Fox
#
##############################################################################

"""implementation of the trie (from reTRIEval, but pronounced "try")
data structure for matching phrases consisting of sequences of words
"""

from Object import Object, OwnerObject
import debug
import string

class WordTrie(Object):
    """trie (from reTRIEval, but pronounced "try") data structure for 
    matching phrases consisting of sequences of words.

    **INSTANCE ATTRIBUTES**
    """
    def __init__(self, **args):
        self.deep_construct(WordTrie, {'branches': {}, 'value': None}, args)

    def add_phrase(self, phrase, value):
        """adds a phrase to the trie and associates it with a value

        Note: if the phrase is already present in the trie, its value
        will be replaced.

        Note: the WordTrie uses the value of None to indicate a
        non-terminal node, so you cannot use None as the value
        corresponding to a phrase

        **INPUTS**

        *[STR] phrase* -- a list of words

        *ANY value* -- the value which will be retrieved  by later
        queries with this phrase

        **OUTPUTS**

        *ANY* -- the old value of that phrase, or None if it didn't
        exist
        """
        debug.trace('WordTrie.add_phrase', 'phrase=%s, value=%s' % (phrase, value))
        
        if not phrase:
            old = self.value
            self.value = value
            return old
        word = phrase[0]
        rest = phrase[1:]
        if not self.branches.has_key(word):
            self.branches[word] = WordTrie() # create a new branch
        self.branches[word].add_phrase(rest, value)

    def match_head(self, phrase):
        """looks for complete or partial matches to the phrase, and
        returns the value corresponding to the longest prefix of the
        given phrase which appears in the WordTrie

        **INPUTS**

        *[STR] phrase* -- list of words to match

        **OUTPUTS**

        *(ANY, [STR])* -- the value corresponding to the partial phrase,
        together with any remaining unmatched words from the phrase
        """
        if not phrase:
            return self.value, []
        word = phrase[0]
        rest = phrase[1:]
        if not self.branches.has_key(word):
            return self.value, phrase
        value, rest = self.branches[word].match_head(rest)
        if value is None:
            return None, phrase
        return value, rest

    def _remove_path(self, phrase):
        """private method which recursively descends the path to the
        value of a given phrase, and, on the way back up, removes those
        portions of that path which are not part of the path to any 
        other value

        **INPUTS**

        *[STR] phrase* -- a list of words

        **OUTPUTS**

        *BOOL* -- true if the portion of the path just explored is no
        longer necessary, because the phrase it leads to was the given
        phrase
        """
        if not phrase:
            self.value = None
            if self.branches:
                return 0
            return 1
        word = phrase[0]
        rest = phrase[1:]
        if not self.branches.has_key(word):
            return 0
        remove = self.branches[word]._remove_path(rest)
        if remove:
            del self.branches[word]
            if self.value is None and self.branches:
                return 1
            return 0
        return 0

    def remove_phrase(self, phrase):
        """remove a phrase from the trie 

        **INPUTS**

        *[STR] phrase* -- a list of words

        **OUTPUTS**

        *BOOL* -- the returned value, or None if there was no complete
        match and the phrase was not removed
        """
        found = self.complete_match(phrase)
        if not (found is None): 
            self._remove_path(phrase)
        return found

    def complete_match(self, phrase):
        """finds the value corresponding to a complete phrase

        **INPUTS**

        *[STR] phrase* -- list of words to match

        **OUTPUTS**

        *ANY* -- the value corresponding to the phrase, or None if the
        phrase was not found
        """
        if not phrase:
            return self.value
        word = phrase[0]
        rest = phrase[1:]
        if not self.branches.has_key(word):
            return []
        return self.branches[word].complete_match(rest)

    def all_matches(self, phrase):
        """returns a list of all complete or partial matches to the phrase

        **INPUTS**

        *[STR] phrase* -- list of words to match

        **OUTPUTS**

        *[(ANY, [STR])]* -- a list of matches in order of decreasing
        completeness.  Each match is a tuple containing the value 
        corresponding to the partial phrase, together with any remaining 
        unmatched words from the phrase
        """
        matches = []
        if phrase:
            word = phrase[0]
            rest = phrase[1:]
            if self.branches.has_key(word):
                matches = self.branches[word].all_matches(rest)
        if not (self.value is None):
            matches.append((self.value, phrase))
        return matches

    def items(self):
        return self.all_phrase_values()

    def all_phrase_values(self, prefix = None):
        """returns a set of all phrases defined in the WordTrie,
        starting (optionally) with a given prefix

        **INPUTS**

        *[STR] prefix* -- optionally, restrict the set of phrases
        returned to those starting with this sequence of words

        **OUTPUTS**

        *[([STR], ANY)]* --  list of (phrase, value) tuples 
        """
        results = []
        if prefix:
            word = prefix[0]
            rest = prefix[1:]
            if not rest and not (self.value is None):
                results.append(([word], self.value))
            if self.branches.has_key(word):
                branch_results = self.branches[word].all_phrase_values(rest)
                for phrase, value in branch_results:
                    results.append(([word] + phrase, value))
            return results
        if not (self.value is None):
            results.append(([], self.value))
        for word, branch in self.branches.items():
            branch_results = branch.all_phrase_values()
            for phrase, value in branch_results:
                results.append(([word] + phrase, value))
        return results

def test_translator(w, sentence):
    phrase = string.split(sentence)
    output = ""
    indentation = 0
    start_of_line = 1
    while phrase:
        translation, rest = w.match_head(phrase)
        space = ''
        if translation is None:
            translation = phrase[0]
#            print start_of_line, indentation, 'word is ', translation
            if start_of_line:
                space = '    ' * indentation
            else:
                space = ' '
            output = output + space
            output = output + translation
            start_of_line = 0
            phrase = phrase[1:]
        else:
            phrase = rest
            for item in translation:
#                print item, type(item)
#                print start_of_line, indentation, 'item is ', item
                if type(item) is type(3):
                    indentation = indentation + item
                else:
                    if start_of_line and indentation:
                        space = '    ' * indentation
                        output = output + space
                    output = output + item
                    if item:
                        if item[-1] == '\n':
                            start_of_line = 1
                        else:
                            start_of_line = 0
    output = output + '\n'
    return output
    
def test_trie():
    w = WordTrie()
    d = {}
    d['if'] = 'if'
    d['return'] = ('return ', -1)
    d['if statement'] = 'if'
    d['else if'] = (-1, '\nelif')
    d['else'] = ('\n', -1, 'else:\n', 1)
    d['else do'] = ('\n', -1, 'else:\n', 1)
    d['else do the following'] = ('\n', -1, 'else:\n', 1)
    d['then'] = (':\n', 1)
    d['then do'] = (':\n', 1)
    d['then do the following'] = (':\n', 1)
    d['greater than'] = ' >'
    d['greater than or equal to'] = ' >='
    d['less than'] = ' <'
    d['less than or equal to'] = ' <='
    d['equals'] = ' ='
    d['is'] = ' is'
    d['is not'] = ' is not'
    d['equal to'] = ' =='
    d['is equal to'] = ' =='
    d['is not equal to'] = ' !='
    d['back tab'] = (-1, )
    d['back indent'] = (-1, )
    d['end block'] = ('\n', -1)
    d['end if'] = ('\n', -1)
    d['end else'] = ('\n', -1)
    d['new line'] = '\n'
    d['zero'] = ' 0'
    d['plus'] = ' +'
    d['minus'] = ' -'
    d['times'] = ' *'
    d['over'] = ' /'
    d['divided by'] = ' /'
    d['open paren'] = ' ('
    d['close paren'] = ' )'
    for key, value in d.items():
        if type(value) is type(""):
            value = (value,)
        w.add_phrase(string.split(key), value)
    sentence = "if x is equal to zero then do raise ValueError"
    sentence = sentence + " else return 1 over x"
    sentence = sentence + " end if"
    output = test_translator(w, sentence)
    print output
    sentence = "if statement y is not equal to x then"
    sentence = sentence + " numerator equals x plus y new line"
    sentence = sentence + " return numerator over"
    sentence = sentence + " open paren x minus y close paren"
    sentence = sentence + " new line"
    sentence = sentence + " return open paren x plus y close paren"
    sentence = sentence + " over sqrt open paren x times y close paren"
    output = test_translator(w, sentence)
    print output
    sentence = "if x equal to n then x equals zero new line back tab"
    sentence = sentence + " print x"
    output = test_translator(w, sentence)
    print output

