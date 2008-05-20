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
# (C) 2003, David Fox
#
##############################################################################

"""Track changes to buffers made by AS_Update objects,
using a fake AppState object"""

import debug
import copy
import re
import string
from Object import Object, OwnerObject

class AppTracker(Object):
    """
    Abstract class which supports the callback methods used by
    AppState update objects, in order to track changes to buffers of
    interest
    """
    def __init__(self, **args):
        self.deep_construct(AppTracker, {}, args)
    def new_window_cbk(self):
        pass
    def suspend_cbk(self):
        pass
    def resume_cbk(self):
        pass
    def find_buff(self, buff_name):
        debug.virtual('AppTracker.find_buff')
    def close_buffer_cbk(self, buff_name):
        debug.virtual('AppTracker.close_buffer_cbk')
    def open_buffer_cbk(self, buff_name):
        debug.virtual('AppTracker.open_buffer_cbk')
    def curr_buff_name_cbk(self, buff_name):
        pass
    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        debug.virtual('AppTracker.rename_buffer_cbk')
        
class BuffTracker(Object):
    """
    Abstract class which supports the callback methods used by
    AppState update objects, in order to track changes to buffers of
    interest
    """
    def __init__(self, buff_name, **args):
        self.deep_construct(BuffTracker, {'buff_name': buff_name}, args)
    def insert_cbk(self, range, text):
        pass
    def delete_cbk(self, range):
        pass
    def name(self):
        return self.buff_name
    def rename_buffer_cbk(self, new_buff_name):
        self.buff_name = new_buff_name
    def pos_selection_cbk(self, pos, selection, visible_range=None):
        pass

class ClosedBuffer(BuffTracker):
    """
    Concrete sub class of BuffTracker representing a buffer which has
    been closed
    """
    def __init__(self, buff_name, old_buffer, **args):
        """
        ** INPUTS **

        *STR buff_name* -- name of the buffer which was closed

        *BuffTracker old_buffer* -- the old BuffTracker, in the state
        it was in before it was deleted

        """
        self.deep_construct(ClosedBuffer, {'original': old_buffer},
                            args)
    def insert_cbk(self, range, text):
        pass
    def delete_cbk(self, range):
        pass
    def name(self):
        return self.buff_name
    def rename_buffer_cbk(self, new_buff_name):
        self.buff_name = new_buff_name
    def pos_selection_cbk(self, pos, selection):
        pass

class DeadBuffer(Exception):
    """
    Exception raised when an update attempts to act on a buffer which
    has previously been closed
    """
    def __init__(self, callback_name, args):
        Exception.__init__(self)
        self.callback_name = callback_name
        self.args = args
        
class ClosedBuffer(BuffTracker):
    """
    Concrete sub class of BuffTracker representing a buffer which has
    been closed
    """
    def __init__(self, buff_name, old_buffer, **args):
        """
        ** INPUTS **

        *STR buff_name* -- name of the buffer which was closed

        *BuffTracker old_buffer* -- the old BuffTracker, in the state
        it was in before it was deleted

        """
        self.deep_construct(ClosedBuffer, {'original': old_buffer},
                            args)
    def insert_cbk(self, range, text):
        raise DeadBuffer(self.name(), 'insert_cbk', {'range': range,
            'text': text})
    def delete_cbk(self, range):
        raise DeadBuffer(self.name(), 'delete_cbk', {'range': range} )
    def name(self):
        return self.buff_name
    def rename_buffer_cbk(self, new_buff_name):
        raise DeadBuffer(self.name(), 'rename_buffer_cbk',
                         {'new_buff_name': new_buff_name} )
    def pos_selection_cbk(self, pos, selection):
        raise DeadBuffer(self.name(), 'pos_selection_cbk')

        
class KnownBuffTracker(AppTracker):
    """
    Partially concrete subclass of AppTracker, which ignores all
    callbacks not related to pre-existing buffers
    """
    def __init__(self, buffers, **args):
        self.deep_construct(KnownBuffTracker,
                            {'buffers': buffers } , args)
    def find_buff(self, buff_name):
        try:
            return self.buffers[buff_name]
        except IndexError:
            return None
    def close_buffer_cbk(self, buff_name):
        original = self.find_buff(buff_name)
        if original:
            self.buffers[buff_name] = ClosedBuffer(buff_name, original)
    def open_buffer_cbk(self, buff_name):
        pass
    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        if old_buff_name == new_buff_name:
            return
        original = self.find_buff(old_buff_name)
        if original:
            original.rename_buffer_cbk(new_buff_name)
            self.buffers[new_buff_name] = original
            del self.buffers[old_buff_name]

class SingleBuffTracker(KnownBuffTracker):
    """
    Partially concrete subclass of KnownBuffTracker, which ignores all
    callbacks not related to a particular buffer
    """
    def __init__(self, buff_name, tracked_buffer, **args):
        self.deep_construct(SingleBuffTracker, {}, args,
                            enforce_value = {'buffers':
                                             {buff_name: tracked_buffer} } )

class TextBlock:
    """
    Class representing a range of text
    """
    def __init__(self, text, start, end = None):
        self.text = text
        self.lower = start
        if end is None:
            self.upper = start + len(text)
        else:
            self.upper = end
    def show(self):
        return "(%d, %d): %s" % (self.lower, self.upper, repr(self.text))
    def inside(self, pos):
        return pos  >= self.lower and pos <= self.upper
    def start(self):
        return self.lower
    def end(self):
        return self.upper
    def len(self):
        return self.upper-self.lower
    def width_change(self):
        return self.len() - len(self.text)
    def shift(self, offset):
        self.lower = self.lower + offset
        self.upper = self.upper + offset
    def overlap(self, range):
        """
        return the region of overlap between the given TextRange and
        this one, if it is non-empty
        """
        start = max(self.start(), range.start())
        end = min(self.end(), range.end())
        if start < end:
            return (start, end)
        else:
            return None
        
    def deletion(self, range):
        """
        Given a range to be deleted, returns a copy of itself has
        shifted and modified by the deletion, together with the remainder
        of the deletion which occurs to the right of this block, and the
        offset to be applied to subsequent blocks.\
        """
        offset = 0
        modified = None
        remaining = None
        if self.end() < range.end():
            remaining = TextBlock('', start = self.end(), end = range.end())
        text = ""
        relative_end = range.end() - self.start()
        relative_start = range.start() - self.start()
        if relative_start < 0:
            offset = - (range.end() - range.start())
            text = self.text[max(relative_end, 0):]
            modified = TextBlock(text, self.start() + offset)
        else:
            text = self.text[0: relative_start]
            text = text + self.text[relative_end:]
            offset = range.start() - min(range.end(), self.end())
            modified = TextBlock(text, self.start())
        return modified, remaining, offset
           

class TextBlocks:
    """
    class representing a series of disjoint blocks of text in order of
    increasing starting point, with adjacent blocks collapsed into a
    single block
    """
    def __init__(self, initial_block = None):
        self.blocks = []
        if initial_block:
            self.blocks = [initial_block]
    def show(self):
        l = []
        for block in self.blocks:
            l.append(block.show() + '\n')
        return l
    def block_containing(self, pos):
        for block in self.blocks:
            if block.inside(pos):
                return block
        return None
            
    def append_block(self, block):
        if not self.blocks:
            self.blocks = [block]
            return 1
        last_block = self.blocks[-1]
        end = last_block.end()
        if block.start() < end:
            return 0
        if block.start() > end:
            self.blocks.append(block)
            return 1
        text = last_block.text + block.text
        self.blocks[ - 1] = TextBlock(text, last_block.start())
               
class BuffInsertionTracker(BuffTracker):
    """
    Concrete subclass of BuffTracker which tracks inserted text,
    combining adjacent and overlapping insertions
    """
    def __init__(self, **args):
        self.deep_construct(BuffInsertionTracker,
                            {'text': TextBlocks()}, args)
    def show(self):
        return self.text.show()
    def insertions(self):
        return copy.deep_copy(self.text)
    def block_containing(self, pos):
        return self.text.block_containing(pos)
    def insert_cbk(self, range, text):
        if range[0] != range[1]:
            self.delete_cbk(range)
        debug.trace('BuffInsertionTracker.insert_cbk',
                    'buff_name = %s' % self.name())
        debug.trace('BuffInsertionTracker.insert_cbk',
                    '%d %d: %s' % (range[0], range[1], repr(text)))
        new_text = TextBlocks()
        offset = 0
        remaining = text
        for block in self.text.blocks:
            next_block = copy.copy(block)
            debug.trace('BuffInsertionTracker.insert_cbk',
                        'next_block: %s' % next_block.show())
            next_block.shift(offset)
            debug.trace('BuffInsertionTracker.insert_cbk',
                        'next_block: %s' % next_block.show())
            if remaining:
                if range[0] >= next_block.end():
                    new_text.append_block(next_block)
                    for block in new_text.blocks:
                        debug.trace('BuffInsertionTracker.insert_cbk',
                                    'block: %s' % block.show())
                    continue
                offset = offset + len(text)
                remaining = None                
                if range[0] <= next_block.start():
                    new_text.append_block(TextBlock(text, range[0]))
                    for block in new_text.blocks:
                        debug.trace('BuffInsertionTracker.insert_cbk',
                                    'block: %s' % block.show())
                    next_block.shift(len(text))
                    debug.trace('BuffInsertionTracker.insert_cbk',
                                'next_block: %s' % next_block.show())
                    new_text.append_block(next_block)
                    for block in new_text.blocks:
                        debug.trace('BuffInsertionTracker.insert_cbk',
                                    'block: %s' % block.show())
                else:
                    t = next_block.text[0:range[0]-next_block.start()]
                    t = t + text
                    t = t + next_block.text[range[0]-next_block.start():]
                    debug.trace('BuffInsertionTracker.insert_cbk',
                                't: %s' % repr(t))
                    new_text.append_block(TextBlock(t, next_block.start()))
                    for block in new_text.blocks:
                        debug.trace('BuffInsertionTracker.insert_cbk',
                                    'block: %s' % block.show())
            else:
                new_text.append_block(next_block)
                for block in new_text.blocks:
                    debug.trace('BuffInsertionTracker.insert_cbk',
                                'block: %s' % block.show())
                
        if remaining:
            new_text.append_block(TextBlock(text, range[0]))
            for block in new_text.blocks:
                debug.trace('BuffInsertionTracker.insert_cbk',
                            'block: %s' % block.show())
        self.text = new_text

    def delete_cbk(self, range):
        debug.trace('BuffInsertionTracker.delete_cbk',
            '%d %d' % range)
        deletion = TextBlock('', range[0], range[1])
        new_text = TextBlocks()
        offset = 0
        for block in self.text.blocks:
            next_block = copy.copy(block)
            debug.trace('BuffInsertionTracker.delete_cbk',
                        'next_block: %s' % next_block.show())
            next_block.shift(offset)
            debug.trace('BuffInsertionTracker.delete_cbk',
                        'next_block: %s' % next_block.show())
            if deletion:
                next_block, deletion, offset = next_block.deletion(deletion)
                debug.trace('BuffInsertionTracker.delete_cbk',
                            'modified block: %d %d, %s' % (next_block.start(),
                            next_block.end(), repr(next_block.text)))
            new_text.append_block(next_block)
            if deletion:
                debug.trace('BuffInsertionTracker.delete_cbk',
                        'deletion: %d %d, %s' % (deletion.start(),
                        deletion.end(), repr(deletion.text)))
            else:
                debug.trace('BuffInsertionTracker.delete_cbk',
                            'no deletion remaining')
            debug.trace('BuffInsertionTracker.delete_cbk',
                        'offset now %d' % offset)
            for block in new_text.blocks:
                debug.trace('BuffInsertionTracker.delete_cbk',
                            'block: %s' % block.show())
        self.text = new_text
    def name(self):
        return self.buff_name
    def rename_buffer_cbk(self, new_buff_name):
        self.buff_name = new_buff_name
    def pos_selection_cbk(self, pos, selection):
        pass
    
class LooseMatch(Object):
    def __init__(self, **args):
        self.deep_construct(LooseMatch, {}, args)
    def expr(self, literal, require_new_lines = 1):
        """
        Given a literal string, returns a regular expression string
        for a loose match.  Non-whitespace characters in the original
        string are escaped so that they match literally (i.e. '.' matches
        '.', not any character).  Consecutive spaces and tabs match any
        string of whitespace.  New lines can be constrained to match new
        lines, or can be allowed to match any whitespace.

        ** INPUTS **

        *STR literal* -- the original, literal string

        *BOOL require_new_lines* -- if true, require new lines in the
        literal string to match new lines.  Otherwise, treat them as any
        other whitespace character

        ** OUTPUTS **

        *STR* -- the regular expression string
        """
        white = 0
        if re.match(r'\s', literal):
            white = 1
        r = re.compile(r'\s+|\S+')
        l = []
        for match in r.findall(literal):
            if white:
                l.append(self.match_white(match, require_new_lines))
                white = 0
            else:
                l.append(re.escape(match))
                white = 1
        return string.join(l, '')
    
    def match_white(self, original, require_new_lines = 1):
        """
        returns a regular expression string matching non-optional
        white space, possibly with embedded required new-line
        characters (if require_new_lines is true and the original
        whitespace string include at least one new line character)
        
        *STR original* -- the original whitespace string

        *BOOL require_new_lines* -- if true, require new lines in the
        literal string to match new lines.  Otherwise, treat them as any
        other whitespace character.  Note: the match will require at
        least as many new-line characters as in the original, but will
        allow for more

        ** OUTPUTS **

        *STR* -- the regular expression string
        """
        if not require_new_lines:
            return r'\s+'
        n = original.count('\n')
        if n == 0:
            return r'\s+'
        optional = r'\s*'
        return optional + n * ('\n' + optional)
    
        
        
                
        
