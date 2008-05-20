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


"""Defines classes, constants, and functions related to spacing of
inserted text.
"""

import sys
import string
import debug
import re
import vc_globals

# spacing flags

normal_spacing = 0x0 
# not really a flag, since we test it with ==, rather than with
# flags | normal_spacing

# fixed flags (i.e. flags whose meaning is not configurable)
no_space_before = 0x1
no_space_after = 0x2
no_space_on = 0x4
no_space_off = 0x8
letters_and_digits = 0x10
joins_identifier = 0x20
# like ( or [, joins a previous identifier (as in "function of" or
# "indexed by"), but not a previous operator (so "x = (").  Note: this
# only affects spacing before - you should still set no_space_after if
# desired

# Explicit spacing: used primarily by built-in words, not our 
# programming-mode LSAs.  However, we need them for compatibility.
hard_space = 0x40 # as in '\space-bar'
hard_new_line = 0x80 # as in '\New-Line'
hard_paragraph = 0x100 # as in '\New-Paragraph'
hard_tab = 0x200 # as in '\tab-key'
end_sentence = 0x400 # as in '.\period'

# configurable flags (categories whose actual behavior depends on
# separately configurable spacing preferences)


binary_operator = 0x1000
# usually like normal spacing (space before and after), but configurable

unary_operator = 0x2000
# usually like no space after, but configurable

unary_or_binary = 0x4000
# for operators like - (minus) which can be unary (if they follow an
# operator, or binary (otherwise).  The spacing treatment of unary and
# binary operators will be configured separately

pre_or_postfix = 0x8000 
# for operators like ++ and -- which can be prefix or postfix.  If they
# follow an identifier or ")", they are assumed to be postfix and will
# have normal spacing after.  Otherwise, they will be assumed to prefix
# and will behave like no_space_after

comparison_operator = 0x10000
# usually normal spacing, but configurable


# common synonyms

no_spaces = no_space_before | no_space_after
like_comma = no_space_before
like_backslash = no_space_before | no_space_after
like_slash = no_space_before | no_space_after
like_bang = unary_operator

like_dot = no_space_before | no_space_after
like_point = no_space_after | letters_and_digits
like_hyphen = no_space_before | no_space_after
like_function_of = no_space_before | no_space_after

like_open_paren = joins_identifier | no_space_after
like_close_paren = no_space_before
like_open_quote = no_space_after
like_close_quote = no_space_before



# defaults for vim - otherwise ignore
# vim:sw=4
