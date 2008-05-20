# Copyright 2006 Lennart Borgman, http://OurComments.org/. All rights
# reserved.
#
# This file is free for personal use. For any other use please contact
# the author.


package html_tags;
use strict;

use vars qw($AUTOLOAD);

sub _make_attributes {
    my($self,$attr) = @_;
    return () unless $attr && ref($attr) && ref($attr) eq 'HASH';
    my(@att);
    foreach (keys %{$attr}) {
	my($key) = $_;
	$key=~s/^\-//;     # get rid of initial - if present
	#$key=~tr/a-z_/A-Z-/; # parameters are upper case, use dashes
	$key=~tr/A-Z_/a-z-/; # parameters are lower case in XHTML
	push(@att,defined($attr->{$_}) ? qq/$key="$attr->{$_}"/ : qq/$key/);
    }
    return @att;
}

sub _tag {
	my $tag_name = shift;
	my $part     = shift;
	my($attr) = '';
	if (ref($_[0]) && ref($_[0]) eq 'HASH') {
		my(@attr) = html_tags::_make_attributes( '',shift() );
		$attr = " @attr" if @attr;
	}
	#return "<$tag_name$attr />" unless @_;
	return "<$tag_name$attr />" if $part == 1;
	return "<$tag_name$attr>"   if $part == 2;
	my($tag,$untag) = ("<$tag_name$attr\n>","</$tag_name\n>");
	my @result = map { "$tag$_$untag" } (ref($_[0]) eq 'ARRAY') ? @{$_[0]} : "@_";
	return $result[0] if $part == 1;
	return "@result";
}

sub _mk_tag_sub($$) {
    my $name    = shift;
    my $package = shift;
    my $caller  = caller;
    my $sep     = ($name =~ s/^\*//);
    my $lc_name   = lc $name;
    my $code =
      ($lc_name =~ m/^(?:br|hr|input|img)$/ ?
    	"sub $package\:\:$name(;\$\$) { return $caller\:\:_tag('$lc_name',1,\@_); }\n"
      :
    	"sub $package\:\:$name(\$;\$) { return $caller\:\:_tag('$lc_name',0,\@_); }\n"
      );
    if ($sep) {
        if ($lc_name eq "html") {
    	    $code .= "sub $package\:\:start_$name(\$;\$\$)
    	    		                        {return $caller\:\:_start_html(\@_);}\n";
    	    $code .= "sub $package\:\:end_$name {return $caller\:\:_end_html();}\n";
        } else {
    	    $code .= "sub $package\:\:start_$name(;\$\$)
    	    					{return $caller\:\:_tag('$lc_name',1,\@_);}\n";
    	    $code .= "sub $package\:\:end_$name {'</$lc_name>';}\n";
        }
    }
    $code;
}
sub _start_html {
	my $title = shift;
	my $head_tags = shift;
	my $body_attr = shift;
	# compensate for perl laziness... (will not detect undef sub)
	$head_tags = $head_tags . _tag("title", 0, $title);
	my $start =
		_tag("html", 2) .
		_tag("head", 0, $head_tags) .
		_tag("body", 2, $body_attr);
}
sub _end_html {
	return '</body></html>';
}

sub header(@) {
	my @lines = @_;
	my $header;
	my $type;
	while (@lines) {
		my $key = shift @lines; my $value = shift @lines;
		$header .= "$key: $value\n";
		$type = $value if $key =~ m/content-type/i;
	}
	$header .= "Content-type: text/html\n" unless defined $type;
	$header .= "\n";
}
sub import {
	shift;
	my %exported;
	$exported{$_}++ for (@_);
	my $caller = caller;
	my $to_eval = "package $caller;\n";
	for my $name (keys %exported) {
		die "Will not redefine $caller\:\:$name" if $caller->can($name);
		my $func;
		if ($name eq "header") {
			$func = "sub header { html_tags::header(); }";
		}
		$func = _mk_tag_sub($name, $caller) unless defined $func;
		$to_eval .= "$func\n";
	}
	eval $to_eval;
	die $@ if $@;
}

1;
