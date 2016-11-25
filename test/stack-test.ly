%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib)                                            %
%              -----------                                                    %
%                                                                             %
% Library: oll-core                                                           %
%          ========                                                           %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% and others.                                                                 %
%       Copyright Jan-Peter Voigt, Urs Liska, 2016                            %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Example file for stack implementation

\pinclude "../package.ly"

% Create an empty <oll:stack> object
mystack = #(oll:stack-create)

% Populate the stack with numbers from 0 to 4
% -> 4 is pushed last so it is on top of the stack
#(for-each
  (lambda (num)
    (oll:push mystack num))
  (iota 5))

#(display "Stack populated with a range of numbers:\n")
#(display mystack)
#(newline)

% read the topmost entry from the stack
#(define top-one (oll:get mystack))

#(display (format "'get' topmost item, remains on stack: ~a\n" top-one))
#(display mystack)#(newline)

% Fetch topmost entry from stack
#(define top-two (oll:pop mystack))
#(display (format "Topmost item, now popped from stack: ~a\n" top-two))
#(display mystack)#(newline)

% Push arbitrary items on top of stack
#(oll:push mystack "hi, how are you?")
#(display "Arbitrary item pushed on top of stack.\n")

#(display mystack)


