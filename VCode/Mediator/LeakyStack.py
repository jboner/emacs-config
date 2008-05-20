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
# (C)2002, National Research Council of Canada
#
##############################################################################

"""stack/queue-like data structures
"""

import debug

import whrandom

import exceptions
from Object import Object

class LeakyStack(Object):
    """a data structure which can act like an unlimited stack, or a
    finite stack, with the oldest elements being dropped off the bottom
    of the stack.

    **INSTANCE ATTRIBUTES**

    *ANY stack* -- the stack

    *INT max_height* -- maximum height of the stack, or None for an
    unlimited stack

    *INT dropped* -- number of items which have been dropped off the
    bottom of the stack
    """
    def __init__(self, max_height = None, **args):
        """
        **INPUTS**

        *INT max_height* -- maximum height of the stack, or None for an
        unlimited stack
        """
        self.deep_construct(LeakyStack,
                            {
                             'max_height': max_height,
                             'dropped':0,
                             'stack': []
                            }, args)
    def push(self, item):
        """push an item onto the top of the stack
        (possibly dropping an old item off the bottom)

        **INPUTS**

        *ANY item* -- the item

        **OUTPUTS**

        *ANY* -- the dropped item, or None if none was dropped
        """
        self.stack.append(item)
        if not (self.max_height is None) and len(self.stack) > self.max_height:
            dropping = self.stack[0]
            del self.stack[0]
            self.dropped = self.dropped + 1
            return dropping

    def height(self):
        """apparent height of the stack (including items which have been
        dropped from the bottom)

        **INPUTS**

        *none*

        **OUTPUTS**
        """
        return len(self.stack) + self.dropped

    def empty(self):
        """tells whether the stack is empty

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the stack is empty
        """
        return len(self.stack) == 0

    def pop(self):
        """pops an item off the top of the stack

        raises an IndexError if the stack is empty

        **INPUTS**

        *none*

        **OUTPUTS**

        *ANY* -- the item on the top of the stack
        """
        return self.stack.pop()

class IndexedLeakyStack(LeakyStack):
    """a LeakyStack which allows you to peek at an item by index from
    the virtual bottom of the stack (taking into account any items which 
    have been dropped of the actual bottom of the stack)

    **INSTANCE ATTRIBUTES**
    """
    def __init__(self, **args):
        """
        """
        self.deep_construct(IndexedLeakyStack,
                            {
                            }, args)

    def lowest(self):
        """returns the index relative to the virtual bottom of the stack
        of the item at the actual bottom

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the index of the lowest remaining item on the stack
        """
        return self.dropped

    def drop_below(self, index):
        """drops items from the bottom of the stack up to (but not including) 
        the given index.  

        **INPUTS**

        *INT index* -- the smallest index to keep

        **OUTPUTS**

        *INT* -- number of items dropped (should always be less than or
        equal to index)
        """
        to_drop = index - self.dropped
        if to_drop > 0:
            del self.stack[0:to_drop]
            self.dropped = self.dropped + to_drop
            return to_drop
        return 0

    def peek(self, index):
        """peek at the item at a given index from the bottom of the
        stack.

        raises an IndexError if the index is too high (above the top of
        the stack) or too low (the item has been dropped)

        **INPUTS**

        *INT index* -- the index of the desired item, relative to the
        (virtual) bottom of the stack, i.e. where the bottom would be if
        the stack were unlimited and no items had been dropped

        **OUTPUTS**

        *ANY* -- a reference to the item
        """
        current = index - self.dropped
        if current < 0:
            raise exceptions.IndexError()
        else:
            return self.stack[current]


class KeyGenerator:
    """source of new, unique (or random) keys
    """
    def new_key(self):
        """return a new key
        """
        debug.virtual('KeyGenerator.new_key')

class KeyGeneratorSequential(KeyGenerator):
    """source of new, unique (or random) keys
    """
    def __init__(self):
        self.next = 0

    def new_key(self):
        """return a new key
        """
        key = self.next
        self.next = self.next + 1
        return key

class KeyGeneratorRandom(KeyGenerator):
    """source of new, unique (or random) keys
    """
    def new_key(self):
        """return a new key
        """
        return repr(whrandom.random())


class KeyedLeakyStack(Object):
    """a data structure like an IndexedLeakyStack (unlimited or finite) 
    but with fast access to the index of a particular item by a key

    **INSTANCE ATTRIBUTES**

    *IndexedLeakyStack stack* -- stack of items

    *IndexedLeakyStack key_stack* -- parallel stack of keys

    *KeyGenerator generator* -- source of new, unique (or random) keys

    *{KEYTYPE: INT} key_index* -- map from keys to index of the 
    corresponding item in the stack
    """
    def __init__(self, generator, max_height = None, **args):
        self.deep_construct(KeyedLeakyStack, 
                {
                 'generator': generator,
                 'stack': IndexedLeakyStack(max_height = max_height),
                 'key_index': {},
                 'key_stack': IndexedLeakyStack(max_height = max_height)
                }, args)

    def height(self):
        """apparent height of the stack (including items which have been
        dropped from the bottom)

        **INPUTS**

        *none*

        **OUTPUTS**
        """
        return self.stack.height()

    def unused_key(self):
        """returns an unused key

        **INPUTS**

        *none*

        **OUTPUTS**

        *KEYTYPE* -- the new key
        """
        try:
            while 1:
                key = self.generator.new_key()
                self.key_index[key]
        except KeyError:
            return key

    def push(self, item):
        """push an item onto the stack

        **INPUTS**

        *ANY item* -- the item

        **OUTPUTS**

        *KEYTYPE* -- the key corresponding to the item
        """
        key = self.unused_key()
        self.stack.push(item)
        key_dropping = self.key_stack.push(key)
        self.key_index[key] = self.stack.height() - 1
        if not (key_dropping is None):
            del self.key_index[key_dropping]
        return key

    def pop(self):
        """pops an item off the top of the stack

        raises an IndexError if the stack is empty

        **INPUTS**

        *none*

        **OUTPUTS**

        *ANY* -- the item on the top of the stack
        """
        key = self.key_stack.pop()
        del self.key_index[key]
        return self.stack.pop()

    def lowest(self):
        """returns the index relative to the virtual bottom of the stack
        of the item at the actual bottom

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the index of the lowest remaining item on the stack
        """
        return self.stack.lowest()

    def index(self, key):
        """finds the index of the item with a given key

        raises a KeyError if the item is not found

        **INPUTS**

        *KEYTYPE key* -- the key

        **OUTPUTS**

        *ANY* -- the item on the top of the stack
        """
        return self.key_index[key]

    def drop_below(self, index):
        """drops items from the bottom of the stack up to (but not including) 
        the given index.  

        **INPUTS**

        *INT index* -- the largest index to drop

        **OUTPUTS**

        *INT* -- number of items dropped (should always be less than or
        equal to index)
        """
        to_drop = index - self.stack.lowest()
        if to_drop > 0:
            for i in range(to_drop):
                key = self.key_stack.peek(i + self.stack.lowest())
                del self.key_index[key]
            self.key_stack.drop_below(to_drop + self.stack.lowest())
            self.stack.drop_below(to_drop + self.stack.lowest())
            return to_drop
        return 0

    def peek(self, index):
        """peek at the item at a given index from the bottom of the
        stack.

        raises an IndexError if the index is too high (above the top of
        the stack) or too low (the item has been dropped)

        **INPUTS**

        *INT index* -- the index of the desired item, relative to the
        (virtual) bottom of the stack, i.e. where the bottom would be if
        the stack were unlimited and no items had been dropped

        **OUTPUTS**

        *ANY* -- a reference to the item
        """
        return self.stack.peek(index)

    def peek_by_key(self, key):
        """peek at the item with a given key

        raises a KeyError if the item is not found

        **INPUTS**

        *KEYTYPE key* -- the key

        **OUTPUTS**

        *ANY* -- a reference to the item
        """
        index = self.index(key)
        return self.peek(index)

   
# defaults for vim - otherwise ignore
# vim:sw=4

