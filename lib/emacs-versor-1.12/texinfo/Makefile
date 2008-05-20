all:	versor.info versor/index.html versor.dvi versor.pdf languide.info languide/index.html languide.dvi languide.pdf

versor.info:	versor.texinfo
	makeinfo versor.texinfo


versor/*.html:	versor.texinfo
	makeinfo --html versor.texinfo

versor.dvi:	versor.texinfo
	tex versor.texinfo
	texindex versor.??
	tex versor.texinfo

versor.pdf:	versor.texinfo
	pdftex versor.texinfo
	texindex versor.??
	pdftex versor.texinfo

languide.info:	languide.texinfo
	makeinfo languide.texinfo


languide/*.html:	languide.texinfo
	makeinfo --html languide.texinfo

languide.dvi:	languide.texinfo
	tex languide.texinfo
	texindex languide.??
	tex languide.texinfo

languide.pdf:	languide.texinfo
	pdftex languide.texinfo
	texindex languide.??
	pdftex languide.texinfo

