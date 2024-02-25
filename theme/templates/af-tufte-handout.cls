\NeedsTeXFormat{LaTeX2e}[1994/06/01]
\ProvidesClass{af-tufte-handout}
  [2023/03/20 v3.6.0 Tufte-handout class]

%%
% Declare we're tufte-handout
% The base LaTeX class (defaults to the article/handout style)
%patch 177
%\newcommand{\@tufte@class}{article}
% The name of the package (defaults to tufte-handout)
%\newcommand{\@tufte@pkgname}{tufte-handout}
\newcommand{\@tufte@class}{article}% the base LaTeX class
% (defaults to the article/handout style; override with 'baseclass' option)
\newcommand{\@tufte@pkgname}{tufte-handout}% the name of the package
%%
% Load the common style elements
\input{af-tufte-common.def}

%%
% Set up any handout-specific stuff now

%%
% Abstract
% TODO The abstract should be printed on its own page of the `titlepage'
% option was specified.
\renewenvironment{abstract}{%
  \begin{quotation}
    \if@tufte@sfsidenotes
      \begin{sffamily}
    \fi
}{%
    \if@tufte@sfsidenotes
      \end{sffamily}
    \fi
  \end{quotation}
}

%%
% If there is a `tufte-handout-local.tex' file, load it.
\IfFileExists{tufte-handout-local.tex}{%
  \input{tufte-handout-local}%
  \@tufte@info@noline{Loading tufte-handout-local.tex}%
}{}

%%
% End of file
\endinput
