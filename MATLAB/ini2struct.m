function Struct = ini2struct(FileName)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Copyright (C) 2019 - LHEEA Res. Dept, Ecole Centrale de Nantes, UMR CNRS 6598
%
%    This program is part of CN-Stream
%
%    CN-Stream is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parses .ini file
% Returns a structure with section names and keys as fields.
%
% Based on init2struct.m by Andriy Nych
% 2014/02/01

f = fopen(FileName,'r');                    % open file
while ~feof(f)                              % and read until it ends
    s = strtrim(fgetl(f));                  % remove leading/trailing spaces
    if isempty(s) || s(1)==';' || s(1)=='#' % skip empty & comments lines
        continue
    end
    if s(1)=='['                            % section header
        Section = genvarname(strtok(s(2:end), ']'));
        Struct.(Section) = [];              % create field
        continue
    end

    [Key,Val] = strtok(s, '=');             % Key = Value ; comment
    Val = strtrim(Val(2:end));              % remove spaces after =

    if isempty(Val) || Val(1)==';' || Val(1)=='#' % empty entry
        Val = [];
    elseif Val(1)=='"'                      % double-quoted string
        Val = strtok(Val, '"');
    elseif Val(1)==''''                     % single-quoted string
        Val = strtok(Val, '''');
    else
        Val = strtok(Val, ';');             % remove inline comment
        Val = strtok(Val, '#');             % remove inline comment
        Val = strtrim(Val);                 % remove spaces before comment

        [val, status] = str2num(Val);       %#ok<ST2NM>
        if status, Val = val; end           % convert string to number(s)
    end

    if ~exist('Section', 'var')             % No section found before
        Struct.(genvarname(Key)) = Val;
    else                                    % Section found before, fill it
        Struct.(Section).(genvarname(Key)) = Val;
    end

end
fclose(f);
