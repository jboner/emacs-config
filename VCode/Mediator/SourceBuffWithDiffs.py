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
# (C)2001, National Research Council of Canada
#
##############################################################################

"""a SourceBuff which implements the state-related methods store_current_state, restore_state, compare_with_current, etc. by storing a history of changes 
to the buffer"""

import debug
from debug import trace, tracing

from SourceBuffCookie import SourceBuffCookie

from SourceBuffCached import SourceBuffCached

from LeakyStack import *

from Object import Object
import find_difference

class ReverseBufferChange(Object):
    """object representing the inverse of a change to a contiguous region of a
    buffer

    **INSTANCE ATTRIBUTES**

    *STR old_text* -- the text which was replaced (and which should be
    restored to undo the change)

    *(INT, INT) *range* -- Start and end position of new text which replaced 
    the old text.
    """
    def __init__(self, old_text, range, **args):
        """
        **INPUTS**

        *STR old_text* -- the text which was replaced (and which should be
        restored to undo the change)

        *(INT, INT) *range* -- Start and end position of new text which 
        replaced the old text.  Note: unlike in SourceBuff methods, the
        range must be specified explicitly.
        """
        self.deep_construct(ReverseBufferChange,
                            {
                             'old_text': old_text,
                             'range': range
                            }, args)

class AccumulatedBufferChange(Object):
    """object representing a change to a contiguous region of a
    buffer

    **INSTANCE ATTRIBUTES**

    *STR text* -- the new text

    *(INT, INT) range* -- Start and end position of the range which was 
    replaced

    *(INT, INT) *new_range* -- Start and end position of the new text
    """
    def __init__(self, text, range, **args):
        """
        **INPUTS**

        *STR text* -- the new text

        *(INT, INT) range* -- Start and end position of the range which was 
        replaced
        """
        self.deep_construct(AccumulatedBufferChange,
                            {
                             'text': text,
                             'range': range,
                             'new_range': None,
                            }, args)
        self.new_range = (range[0], range[0] + len(text))

    def compose(self, next):
        """composes this change with the next one, if the regions
        changed by the two changes are contiguous but disjoint

        **INPUTS**

        *AccumulatedBufferChange next* -- the immediately subsequent change

        **OUTPUTS**

        *AccumulatedBufferChange* -- the composition of the two, or None if
        this method cannot create it because the two regions were not
        contiguous and disjoint.
        
        Note: in principle, any two contiguous changes can be expressed 
        in terms of a larger one which encompasses the entire region 
        changed by either, but that requires the full text of the 
        buffer.  Here, we're only dealing with the simplest case 
        where the entire region deleted/replaced by either change is
        contiguous and distinct
        """
# special case needed for Emacs - not sure why
        if next.range[0] == next.range[1] and next.text == "":
            return AccumulatedBufferChange(self.text, self.range)
        if next.range[0] == self.new_range[1]:
            text = self.text + next.text
            start = self.range[0]
            end = self.range[1] + (next.range[1] - next.range[0])
            return AccumulatedBufferChange(text, range = (start, end)) 
        if next.range[1] == self.new_range[0]:
            text = next.text + self.text
            start = next.range[0]
            end = self.range[1]
            return AccumulatedBufferChange(text, range = (start, end)) 
        return None


class DiffCookie(SourceBuffCookie):
    """SourceBuffCookie subclass used by SourceBuffWithDiffs

    **INSTANCE ATTRIBUTES**

    *STR* cookie_key -- a unique ID which allows SourceBuffWithDiffs to
    match the cookie with its corresponding data
    """
    def __init__(self, buff_name, cookie_key, **args):
        self.deep_construct(DiffCookie,
                            {
                             'buff_name': buff_name,
                             'cookie_key': cookie_key
                            }, args)

    def rename_buffer_cbk(self, new_buff_name):
        """callback which notifies us that the application
        has renamed the buffer corresponding to this cookie

        **INPUTS**

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        self.buff_name = new_buff_name

class CookieData(Object):
    """underlying data corresponding to a cookie
    
    **INSTANCE ATTRIBUTES**

    *INT* level -- level from the (virtual) bottom of the stack of 
    ReverseBufferChange objects to which we must undo the changes in order 
    to restore the buffer to the state at the time this cookie was created 

    *INT* cursor_at_end -- indicates whether the cursor is at the start (0)
    or end (1) of the selection_range

    *(INT, INT)* selection_range -- range of the selection

    *last_search* -- last logged search (see SourceBuff.log_search)
    """
    def __init__(self, level, selection, cursor_at = 1,
                 last_search = None, **args):
        """
        **INPUTS**

        *INT* level -- level from the (virtual) bottom of the stack of 
        ReverseBufferChange objects down to which we must undo the changes in order 
        to restore the buffer to the state at the time this cookie was created 

        *(INT, INT)* selection -- range of the selection.  Unlike
        in some SourceBuff methods, the selection must be specified
        explicitly.

        *INT* cursor_at -- indicates whether the cursor is at the start (0)
        or end (1) of the selection_range

        *last_search* -- last logged search (see SourceBuff.log_search)
        """
        self.deep_construct(CookieData,
                            {
                             'level': level,
                             'selection_range': selection,
                             'cursor_at_end': cursor_at,
                             'logged_search': last_search
                            }, args)

    def get_selection(self):
        """retrieves range of stored selection.  

        **INPUTS**

        *none*
        
        **OUTPUTS**

        *(INT, INT)* -- (start, end)

        start is the offset into the buffer of the start of the current
        selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).
        """
        return self.selection_range

    def position(self):
        """returns the stored location of the cursor

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- offset into the buffer of the stored cursor location
        """
        return self.selection_range[self.cursor_at()]

    def cursor_at(self):
        """tells at which end of the selection the cursor was located

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- 1 if cursor is at the end of the selection, 0 if it is
        at the start
        """
        return self.cursor_at_end
    
    def last_search(self):
        return self.logged_search


class SourceBuffWithDiffs(SourceBuffCached):
    """a subclass of SourceBuffCached which implements the 
    state-related methods store_current_state, restore_state, 
    compare_with_current, etc. by storing a history of changes 
    to the buffer

    Note: all callbacks which report changes to the buffer must be
    implemented in this class as well as SourceBuffCached.

    **INSTANCE ATTRIBUTES**

    *IndexedLeakyStack[ReverseBufferChange] change_history* -- BufferChange
    objects which are the inverses of changes which have been made to
    the buffer, with the most recent change last

    *KeyedLeakyStack[CookieData] cookie_jar* -- stack of CookieData items
    corresponding to stored cookies

    *BOOL* undoing -- flag indicating whether we are currently undoing
    changes

    *[AccumulatedBufferChange]* accumulated -- if currently undoing 
    changes, this is the list of accumulated changes

    *INT max_cookies* -- the maximum number of cookies (and thus states)
    to store, or None to allow an unlimited undo and reinterpretation.
    """
    def __init__(self, max_cookies = None, **args):
        """
        *INT max_cookies* -- the maximum number of cookies (and thus states)
        to store, or None to allow an unlimited undo and reinterpretation.
        """
        self.deep_construct(SourceBuffWithDiffs,
                            {
                             'change_history':
                                 IndexedLeakyStack(),
                             'cookie_jar':
                                 KeyedLeakyStack(generator = \
                                     KeyGeneratorRandom(), 
                                     max_height = max_cookies),
                             'max_cookies': max_cookies,
                             'undoing': 0,
                             'accumulated': None
                            },
                            args
                            )

    def clear_stacks(self):
        """clear stacks because the change history has been invalidated.

        Normally, this method is only called by other
        SourceBuffWithDiffs methods.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('SourceBuffWithDiffs', 'clearing stacks')
        self.change_history = IndexedLeakyStack()
        self.cookie_jar = KeyedLeakyStack(generator = KeyGeneratorRandom(), 
            max_cookies = self.max_cookies)

    def push_cookie(self, data):
        """push a cookie onto the cookie_jar stack, while also
        dropping obsolete changes from the change_history.

        **INPUTS**

        *CookieData data* -- the data to be pushed onto the cookie_jar
        stack

        **OUTPUTS**

        *STR* -- the key returned by the cookie jar (used to identify
        the cookie with its data)
        """
        key = self.cookie_jar.push(data)
        i = self.cookie_jar.lowest()
        bottom_cookie = self.cookie_jar.peek(i)
        try:
            level = bottom_cookie.level
            self.change_history.drop_below(level)
        except AttributeError:
            pass
        return key

    def _push_change(self, change):
        """ private method for pushing a new ReverseBufferChange object onto
        the change_history stack

        **INPUTS**

        *ReverseBufferChange change* -- the object representing the reverse
        diff (would undo the change the editor reported)

        **OUTPUTS**

        *none*
        """
        if tracing('SourceBuffWithDiffs._push_change'):
            debug.trace('SourceBuffWithDiffs._push_change',
                'change to buff %s: old text "%s", replaced range = %s' \
                % (self.name(), change.old_text, repr(change.range)))
        dropped = self.change_history.push(change)
        if dropped:
            if tracing('SourceBuffWithDiffs._push_change'):
                debug.trace('SourceBuffWithDiffs._push_change',
                    'dropped old text "%s", replaced range = %s' \
                    % (dropped.old_text, repr(dropped.range)))
# level of the dropped change was:
            dropped_level = self.change_history.lowest()
            debug.trace('SourceBuffWithDiffs._push_change',
                'dropped level %d' % dropped_level)
# look through the cookie stack for cookies which referred to changes at or
# below the dropped level, and drop them, since they are no longer valid
            lowest_cookie = self.cookie_jar.lowest()
            jar_height = self.cookie_jar.height()
            debug.trace('SourceBuffWithDiffs._push_change',
                'cookie jar lowest, height = %d, %d' % (lowest_cookie,
                jar_height))
            debug.trace('SourceBuffWithDiffs._push_change',
                'looking through cookie jar...')
            found = 0
            for i in range(lowest_cookie, jar_height):
                cookie_data = self.cookie_jar.peek(i)
                level = cookie_data.level
                debug.trace('SourceBuffWithDiffs._push_change',
                    'at index %d, level = %d' % (i, level))
                if level > dropped_level:
                    debug.trace('SourceBuffWithDiffs._push_change',
                        'found level')
                    found = 1
                    break
            if found:
                debug.trace('SourceBuffWithDiffs._push_change',
                    'dropping cookies below index %d' % i)
                dropped_cookies = self.cookie_jar.drop_below(i)
                debug.trace('SourceBuffWithDiffs._push_change',
                    'dropped %d cookies' % dropped_cookies)
            else:
                debug.trace('SourceBuffWithDiffs._push_change',
                    'dropping cookies below height' % i)
                self.cookie_jar.drop_below(self.cookie_jar.height())
                debug.trace('SourceBuffWithDiffs._push_change',
                    'dropped %d cookies' % dropped_cookies)

    def during_undo(self, text, range):
        """while undoing, accumulates consecutive changes so we can
        compare them with what was expected

        **INPUTS**

        *STR text* -- the new text

        *(INT, INT) range* -- Start and end position of the range which was 
        replaced

        **OUTPUTS**
        
        *none*
        """
        change = AccumulatedBufferChange(text, range)
        if not self.accumulated:
            self.accumulated = [change]
        else:
            new_last = self.accumulated[-1].compose(change)
            if new_last:
                self.accumulated[-1] = new_last
            else:
                self.accumulated.append(change)

    def no_change(self, lower, upper = None):
        """determines whether there has been any net change in the
        buffer between a starting and ending level in the change
        history.

        **INPUTS**

        *INT lower* -- starting level in change_history 

        *INT upper* -- ending level in change_history, or None to
        compare with the current state

        **OUTPUTS**

        *BOOL* -- true if there has been no change
        """
# for now, we don't attempt to find the composition of the changes.
# Instead, we simply assume that only an empty range isdo the easiest thing
        if upper is None:
            upper = self.change_history.height()
        debug.trace('SourceBuffWithDiffs.no_change',
            'lower, upper = %d, %d' % (lower, upper))
        return lower == upper


    #
    # Callback methods. These are invoked by the external editor to notify
    # VoiceCode that certain events have taken place in the editor.
    #
    def delete_cbk(self, range):
        """External editor invokes that callback to notify VoiceCode
        of a deletion event.

        **INPUTS**
        
        (INT, INT) *range* -- Start and end pos of range to be deleted
        

        **OUTPUTS**
        
        *none* -- 
        """
        debug.trace('SourceBuffWithDiffs.delete_cbk',
            'buff %s: deleting range = %s' % (self.name(), repr(range)))
        if self.undoing:
            debug.trace('SourceBuffWithDiffs.delete_cbk',
                'in process of undoing')
            self.during_undo(text = "", range = range)
        else:
            if self.cache['get_text'] == None:
# if we don't have the buffer contents cached, we don't know what text 
# was deleted, so we can't create a reverse diff, and all our previous
# change_history is invalid
                debug.trace('SourceBuffWithDiffs.delete_cbk',
                    'not cached')
                if range[1] != range[0] and range[1] != range[0] + 1:
                    debug.trace('SourceBuffWithDiffs.delete_cbk',
                        'non-empty change')
                    self.clear_stacks()
            else:
# we need the old text, so we have to do all this processing before
# calling SourceBuffCached.delete_cbk
                deleted = self.cache['get_text'][range[0]:range[1]]
# don't record deletions of nothing
                if tracing('SourceBuffWithDiffs.delete_cbk'):
                    debug.trace('SourceBuffWithDiffs.delete_cbk',
                        'deleted text "%s"' % deleted)
                if deleted:
# for the reverse diff, we need the range of the new text
# The start of the new text is the same as the start of the old text,
# but the end is offset from the start by one more than the length of
# the new text
                    start = range[0]
                    end = range[0]
# for deletions, the range of the new text is empty (technically, we 
                    reverse = ReverseBufferChange(deleted, (start, end))
                    self._push_change(reverse)

        SourceBuffCached.delete_cbk(self, range)

    def insert_cbk(self, range, text):
        
        """External editor invokes that callback to notify VoiceCode
        of a deletion event.

        **INPUTS**
                
        (INT, INT) *range* -- Start and end position of text to be
        replaced by the insertion. If end of range is None, default to 
        the end of the buffer.

        STR *text* -- Text to be inserted

        **OUTPUTS**
        
        *none* -- 
        """
        if tracing('SourceBuffWithDiffs.insert_cbk'):
            debug.trace('SourceBuffWithDiffs.insert_cbk',
                'buff %s: replacing range %s with "%s"' \
                % (self.name(), repr(range), text))
        if self.undoing:
            debug.trace('SourceBuffWithDiffs.insert_cbk',
                'in process of undoing')
            self.during_undo(text = text, range = range)
        else:
            if self._not_cached('get_text'):
# if we don't have the buffer contents cached, we don't know what text 
# was replaced, so we can't create a reverse diff, and all our previous
# change_history is invalid
                debug.trace('SourceBuffWithDiffs.insert_cbk',
                    'not cached')
                if range[1] != range[0] and range[1] != range[0] + 1:
                    debug.trace('SourceBuffWithDiffs.insert_cbk',
                        'non-empty change')
                    self.clear_stacks()
            else:
# we need the old text, so we have to do all this processing before
# calling SourceBuffCached.insert_cbk
                range_non_nil = [range[0], range[1]]
                if range_non_nil[1] == None:
                   range_non_nil[1] = len(self._get_cache('get_text')) - 1
                replaced = self.cache['get_text'][range_non_nil[0]:range_non_nil[1]]
                if tracing('SourceBuffWithDiffs.insert_cbk'):
                    debug.trace('SourceBuffWithDiffs.insert_cbk',
                        'replaced text "%s"' % replaced)
# don't record non-changes
                if replaced != text:
# for the reverse diff, we need the range of the new text
# The start of the new text is the same as the start of the old text,
# but the end is offset from the start by one more than the length of
# the new text
                    start = range_non_nil[0]
# we use the same convention for ranges as Python's slice
                    end = start + len(text)
                    reverse = ReverseBufferChange(replaced, (start, end))
                    self._push_change(reverse)

        SourceBuffCached.insert_cbk(self, range, text)

    def contents_cbk(self, text):
        
        """External editor invokes that callback to inform VoiceCode
        of the buffer contents.

        **INPUTS**
                
        STR *text* -- Text to be inserted

        **OUTPUTS**
        
        *none* -- 
        """
        if self._not_cached('get_text'):               
# if we don't have the buffer contents cached, we don't know what text 
# was replaced, so we can't create a reverse diff, and all our previous
# change_history is invalid
            debug.trace('SourceBuffWithDiffs.contents_cbk',
                'not cached')

# if the text isn't cached, then the stacks should already be clear,
# but just for good measure
            self.clear_stacks()
            if self.undoing:
                # this should REALLY never happen, but if it does,
                # there is no good way to handle it
                msg = 'WARNING: SourceBuffWithDiffs.contents_cbk called\n'
                msg = msg + 'while undoing changes, with NO CACHED TEXT.\n'
                msg = msg +'.  Please report this as a bug\n'
                debug.critical_warning(msg)
            SourceBuffCached.contents_cbk(self, text)
            return
        
# otherwise, treat this as an insert callback
        start, end, change = \
               find_difference.find_difference(self.cache['get_text'], text)
        self.insert_cbk(range = (start, end), text = change)

    def _state_cookie_class(self):
        """returns the class object for the type of cookie used by
        store_current_state.

        **INPUTS**

        *none*

        **OUTPUTS**

        *CLASS* -- class of state cookies corresponding to this
        SourceBuff

        """
        return DiffCookie
        
    def store_current_state(self):
        """stores the current state of the buffer, including both the
        contents and the current selection, for subsequent restoration.
        Store_current_state returns a "cookie" which can be passed to
        restore_state or compare_with_current.  The type and attributes
        of the cookie will depend on the specific subclass of
        SourceBuff.  In the most straightforward implementation, it 
        may include a copy of the entire contents of the
        buffer and the selection.  In other cases, particularly when the
        editor or SourceBuff provides an internal undo stack, it may simply be a
        reference to a point in this stack.
        
        Important Notes:
        
        You should only pass the cookie to methods of
        the SAME SourceBuff object from which it came.  Generally,
        cookies can not be pickled and retrieved.

        The type of cookie will vary with the concrete subclass 
        of SourceBuff.  The corresponding class object is 
        returned by _state_cookie_class.  However, external callers
        should not depend on the type, attributes, or methods 
        of the cookie.

        This method does not synchronize with the editor prior to
        storing the state.  The caller is responsible for synchronizing 
        if desired.  (This avoids having duplicate synchronize calls 
        when storing the current state of more than one buffer).

        **INPUTS**

        *none*

        **OUTPUTS**

        *DiffCookie* -- state cookie (see above)
        """
        level = self.change_history.height()
        debug.trace('SourceBuffWithDiffs.store_current_state',
            'current level is %d' % level)
        selection = self.get_selection()
        pos = self.cur_pos()
        if pos == selection[0]:
            cursor_at = 0
        else:
            cursor_at = 1
        data = CookieData(level, selection, cursor_at = cursor_at,
                          last_search = self.last_search)
        key = self.push_cookie(data)
        debug.trace('SourceBuffWithDiffs.store_current_state',
            'key is %s' % key)
        return DiffCookie(buff_name = self.name(), cookie_key = key)

    def restore_state(self, cookie):
        """restores the buffer to its state at the time when
        the cookie was returned by store_current_state.  Both the
        contents and the selection will be restored.  However, other
        data, such as the search history, may not.  The restore
        operation can fail, which will be indicated by a return value of
        0, so the caller should always check the return value.
        
        **INPUTS**

        *SourceBuffCookie cookie* -- see above.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if restore was successful

        """
        if not self.valid_cookie(cookie):
            return 0
        if not (self.accumulated is None):
            debug.trace('SourceBuffWithDiffs.restore_state',
                'already inside a restore_state call')
            return 0
        debug.trace('SourceBuffWithDiffs.restore_state',
            'key is %s' % cookie.cookie_key)
        index = self.cookie_jar.index(cookie.cookie_key)
        debug.trace('SourceBuffWithDiffs.restore_state',
            'at index %d in the cookie jar' % index)
        data = self.cookie_jar.peek(index)

        level = data.level
        debug.trace('SourceBuffWithDiffs.restore_state',
            'data.level = %d out of %d' % \
            (level, self.change_history.height()))
# later, we may put this stuff on a forward stack, but for now, just pop
# it
        while self.cookie_jar.height() > index:
            debug.trace('SourceBuffWithDiffs.restore_state',
                'cookie jar height = %d, popping' \
                    % self.cookie_jar.height())
            self.cookie_jar.pop()

        self.undoing = 1
        success = 0
        try:
            while self.change_history.height() > level:
                debug.trace('SourceBuffWithDiffs.restore_state',
                    'change history height = %d, popping' \
                    % self.change_history.height())
                change = self.change_history.pop()
                text = change.old_text
                start, end = change.range
                self.accumulated = []
                if tracing('SourceBuffWithDiffs.restore_state'):
                    debug.trace('SourceBuffWithDiffs.restore_state',
                        'popped text "%s", range = (%d, %d)' \
                        % (text, start, end))
                self.set_text(text, start = start, end = end)
# the callback should clear this if we got the expected change.
# if it didn't, then we're in trouble.  Since we don't have a forward
# stack yet, there's nothing we can do except return false to indicated
# failure
                if len(self.accumulated) != 1:
                    break
                accumulated_text = self.accumulated[0].text
                accumulated_range = self.accumulated[0].range
                if text != accumulated_text:
                    if tracing('SourceBuffWithDiffs.restore_state'):
                        debug.trace('SourceBuffWithDiffs.restore_state',
                           'text "%s" != expected "%s"' % (accumulated_text, text))
                    break
                if change.range != accumulated_range:
                    debug.trace('SourceBuffWithDiffs.restore_state',
                       'range %s != expected %s' \
                       % (repr(accumulated_range), repr(change.range)))
                    break
            success = 1
        finally:
            self.undoing = 0
            self.accumulated = None
            if success:
                self.set_selection(data.get_selection(), 
                    cursor_at = data.cursor_at())
                self.last_search = data.last_search()
            self.print_buff_if_necessary()
            return success

    def compare_states(self, first_cookie, second_cookie, selection = 0):
        """compares the buffer states at the times when
        two cookies were returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to either cookie has
        been lost, compare_states will return false.

        This method does not synchronize with the editor prior to
        comparing with "current".  To ensure that the "current" state 
        is really current, the caller must synchronize.
        (This avoids having duplicate synchronize calls 
        when comparing with the current state of more than one buffer).

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if states are the same, false if they are not, or
        it cannot be determined due to expiration of either cookie
        """
        if not self.valid_cookie(first_cookie) or not \
            self.valid_cookie(second_cookie):
            return 0
        index = self.cookie_jar.index(first_cookie.cookie_key)
        data = self.cookie_jar.peek(index)
        level = data.level
        second_index = self.cookie_jar.index(second_cookie.cookie_key)
        second_data = self.cookie_jar.peek(second_index)
        second_level = second_data.level

        if not self.no_change(level, second_level):
            debug.trace('SourceBuffWithDiffs.compare_states',
                'non-empty list of diffs')
            return 0
        if selection:
            return self.compare_state_selections(first_cookie, second_cookie)
        return 1

    def compare_state_selections(self, first_cookie, second_cookie):
        """compares the selection and cursor positions at the times when
        two cookies were returned by store_current_state.
        If the state corresponding to either cookie has
        been lost, compare_states will return false.

        This method does not synchronize with the editor prior to
        comparing with "current".  To ensure that the "current" state 
        is really current, the caller must synchronize.
        (This avoids having duplicate synchronize calls 
        when comparing with the current state of more than one buffer).

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or it cannot be determined due to expiration of 
        either cookie
        """
        if not self.valid_cookie(first_cookie) or not \
            self.valid_cookie(second_cookie):
            return 0
        pos, selection = self.get_state_pos_selection(first_cookie)
        second_pos, second_selection = \
            self.get_state_pos_selection(second_cookie)
        if pos != second_pos or selection != second_selection:
            return 0
        return 1

    def compare_with_current(self, cookie, selection = 0):
        """compares the current buffer state to its state at the time when
        the cookie was returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if state is the same, false if it is not, or
        it cannot be determined due to expiration of the cookie
        """
        if not self.valid_cookie(cookie):
            debug.trace('SourceBuffWithDiffs.compare_with_current',
                'invalid cookie')
            return 0
        index = self.cookie_jar.index(cookie.cookie_key)
        data = self.cookie_jar.peek(index)

        level = data.level
        if not self.no_change(level):
            debug.trace('SourceBuffWithDiffs.compare_with_current',
                'non-empty list of diffs')
            return 0
        if selection:
            return self.compare_selection_with_current(cookie)
        return 1

    def compare_selection_with_current(self, cookie):
        """compares the current buffer position and selection to these
        values at the time when the cookie was returned by 
        store_current_state.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or if it cannot be determined due to 
        expiration of the cookie
        """
        if not self.valid_cookie(cookie):
            debug.trace('SourceBuffWithDiffs.compare_selection_with_current',
                'invalid cookie')
            return 0
        index = self.cookie_jar.index(cookie.cookie_key)
        data = self.cookie_jar.peek(index)

        if data.get_selection() != self.get_selection():
            debug.trace('SourceBuffWithDiffs.compare_selection_with_current',
                'selections differ: %s, %s' % \
                (repr(data.get_selection()), repr(self.get_selection())))
            return 0
        if data.position() != self.cur_pos():
            debug.trace('SourceBuffWithDiffs.compare_selection_with_current',
                'positions differ: %d, %d' % (data.position(), self.cur_pos()))
            return 0
        return 1

    def get_state_pos_selection(self, cookie):
        """retrieves the position and selection from a given state
        cookie.  

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *(INT, (INT, INT))* -- position and selection at the time the
        cookie was created by store_current_state, or None if the cookie
        is invalid (usually because the state corresponding to the cookie 
        has been lost).
        """
        if not self.valid_cookie(cookie):
            return 0
        index = self.cookie_jar.index(cookie.cookie_key)
        data = self.cookie_jar.peek(index)

        selection = data.get_selection() 
        pos = data.position() 
        return (pos, selection)

    def valid_cookie(self, cookie):
        """checks whether a state cookie is valid or expired.
        If the state corresponding to the cookie has
        been lost, valid_cookie will return false.

        **INPUTS**

        *DiffCookie cookie* -- see store_current_state.  

        **OUTPUTS**

        *BOOL* -- true if cookie is valid (i.e. restore_state should be
        able to work)
        """
        if not issubclass(cookie.__class__, self._state_cookie_class()):
            debug.trace('SourceBuffWithDiffs.valid_cookie',
                'cookie has wrong class')
            return 0

        if cookie.buff_name != self.name():
            debug.trace('SourceBuffWithDiffs.valid_cookie',
                'cookie has wrong buffer name')
            return 0

# ResMgrBasic and BufferStatesBasic sometimes store an extra two states
# before restoring.  We want to make sure that the cookies which are
# valid when they check the safe depth are still valid when they go to
# restore, so we push an extra two cookies now, so that we have headroom
# on the leaky cookie_jar stack
        self.push_cookie('dummy')
        self.push_cookie('dummy')
        self.cookie_jar.pop()
        self.cookie_jar.pop()

# cookie's data must still be stored
        try:
            index = self.cookie_jar.index(cookie.cookie_key)
        except KeyError:
            debug.trace('SourceBuffWithDiffs.valid_cookie',
                'unknown cookie key %s' % cookie.cookie_key)
            return 0
        try:
            data = self.cookie_jar.peek(index)
        except IndexError:
            debug.trace('SourceBuffWithDiffs.valid_cookie',
                'that cookie not found in the cookie jar')
            return 0

# corresponding level in change_history must still be there
        level = data.level
        if level < self.change_history.lowest():
            debug.trace('SourceBuffWithDiffs.valid_cookie',
                'diffs to cookie level have been dropped')
            return 0
        if level > self.change_history.height():
            debug.trace('SourceBuffWithDiffs.valid_cookie',
                'diffs to cookie level have been popped')
            return 0
        return 1


# defaults for vim - otherwise ignore
# vim:sw=4
