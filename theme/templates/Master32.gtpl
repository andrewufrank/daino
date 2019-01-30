<!DOCTYPE html>
<!-- a master page for the pandoc templating mechanism includes a body page 
	expects body inserted from second template using the glabrous templating mechanis
	needs page-title, page-title-postfix
		author date and keywords for indexing
	uses style.css as default, other can be loaded 
	-->
<html lang="$lang$">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes" />

    <link rel="stylesheet" type="text/css" href=static/style.css">

  <style type="text/css">
      code{white-space: pre-wrap;}
      span.smallcaps{font-variant: small-caps;}
      span.underline{text-decoration: underline;}
      div.column{display: inline-block; vertical-align: top; width: 50%;}
	$if(quotes)$
      	q { quotes: "“" "”" "‘" "’"; }
	$endif$
  </style>
	$if(highlighting-css)$
	  <style type="text/css">
	$highlighting-css$
	  </style>
	$endif$

	$for(css)$
	  <link rel="stylesheet" href="$css$" />
	$endfor$

    $for(author)$
      <meta name="author" content="$author$" />
    $endfor$
    $if(date)$
      <meta name="dcterms.date" content="$date$" />
    $endif$
    $if(keywords)$
      <meta name="keywords" content="$for(keywords)$$keywords$$sep$, $endfor$" />
    $endif$

	<title>$page-title$$if(page-title-postfix)$--$page-title-postfix$$endif$</title>

<!--    <title>$if(title-prefix)$$title-prefix$ – $endif$$pagetitle$</title>
	$if(math)$
	  $math$
	$endif$
	$for(header-includes)$
	  $header-includes$
	$endfor$
-->
  </head>
 
{{body}}
 
</html>
