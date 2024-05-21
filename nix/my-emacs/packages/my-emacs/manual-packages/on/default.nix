# Copyright (C) 2022-2024 Burgess Chang

# This file is part of my-emacs.

# my-emacs is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# my-emacs is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with my-emacs.  If not, see <https://www.gnu.org/licenses/>.
{
  fetchFromGitLab,
  lib,
  trivialBuild,
}:
trivialBuild rec {
  pname = "on.el";

  version = "0.1.0";

  src = fetchFromGitLab {
    owner = "ajgrf";
    repo = "on.el";
    rev = "3cf623e1a4331e259ef92e49154ed0551f300436";
    hash = "sha256-gtSVCpQwv4Ui9VpW7SXnsXIkfHN/6laMLqHTezDcMZg=";
  };

  meta = with lib; {
    description = "utility hooks and functions from Doom Emacs";
    homepage = "https://gitlab.com/ajgrf/on.el";
    license = licenses.mit;
  };
}
