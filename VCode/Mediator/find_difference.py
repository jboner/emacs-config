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

"""misc. utilities related to finding text in strings
"""


import string

def find_difference(old, new):
    """Finds the difference between old and new sequences, assuming that there
    is only one contiguous changed region.

    **INPUTS**

    *SEQ ANY* old -- old sequence

    *SEQ ANY* new -- new sequence
    
    **OUTPUTS**

    (start, end, change)
    
    *INT* start -- the offset into old of the range replaced or deleted

    *INT* end -- the offset into old of the item following 
    the range modified (or deleted) (this matches Python's slice convention).

    *ANY* change -- the replacement range from new
    """
#     print repr(old), repr(new)
    shorter = min(len(old), len(new))
    longer = max(len(old), len(new))
    for i in range(shorter):
        if old[i] != new[i]: break
    else:
# we reached the end of the shorter sequence
#        print 'extra ', shorter, longer, repr(new[shorter:])
        return shorter, longer, new[shorter:]
# otherwise
#    print 'difference at ', i
    rest = shorter - i
    for j in range(rest):
        if old[-1-j] != new[-1-j]: break
#    print 'full', i, len(old) - j - 1, repr(new[i:-j-1])
    return i, len(old)-j-1, new[i:-j-1]
    
def find_string_difference(old, new):
    """Finds the difference between old and new strings, assuming that there
    is only one contiguous changed region.

    **INPUTS**

    *STR* old -- old string

    *STR* new -- new string
    
    **OUTPUTS**

    (start, end, change)
    
    *INT* start -- the offset into old of the range replaced or deleted

    *INT* end -- the offset into old of the character following 
    the range modified (or deleted) (this matches Python's slice convention).

    *STR* change -- the replacement string from new
    """
# note, this function is probably terribly slow for long strings.
# We may want to rewrite find_difference as an extension module in C
    return find_difference(old, new)
    
def find_count(s, sub, count = 1, start = None, end = None):
    """Generalizes string.find to find the count-th non-overlapping occurrence 
    of sub in s[start:end]

    **INPUTS**

    *STR* s -- string to search

    *STR* sub -- look for sub

    *INT* count -- ignore the first count-1 occurences

    *INT* start, end -- look in s[start:end]

    **OUTPUTS**

    *INT* -- index of count-th occurence, or -1 if there are fewer
    than count occureneces

    """
    first, last = start, end
    if first == None:
        first = 0
    if last == None:
        last = len(s)
    for i in range(count - 1):
        found = string.find(s, sub, first, last)
        if found == -1:
            return found
        first = found + len(sub)
    return string.find(s, sub, first, last)
                
    
    
def rfind_count(s, sub, count = 1, start = None, end = None):
    """Generalizes string.rfind to find the count-th non-overlapping occurrence 
    of sub from the end of s[start:end]

    **INPUTS**

    *STR* s -- string to search

    *STR* sub -- look for sub

    *INT* count -- ignore the first count-1 occurences

    *INT* start, end -- look in s[start:end]

    **OUTPUTS**

    *INT* -- index of count-th occurence, or -1 if there are fewer
    than count occureneces

    """
    first, last = start, end
    if first == None:
        first = 0
    if last == None:
        last = len(s)
    for i in range(count - 1):
        found = string.rfind(s, sub, first, last)
        if found == -1:
            return found
        last = found
    return string.rfind(s, sub, first, last)
                
    
