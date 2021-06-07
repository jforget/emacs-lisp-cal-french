#!/usr/bin/emacs --script
; -*- encoding: utf-8; indent-tabs-mode: nil -*-
;
; Generated test file for the Emacs cal-french.el program.
; Fichier de test généré pour tester le programme cal-french.el de Emacs
;
; Copyright (C) 2014, 2021 Jean Forget
;
; Author: Jean Forget
; Maintainer: Jean Forget
; Keywords: French Revolution, calendar
;
; This program is free software; you can redistribute it and modify
; it under the terms of Emacs:
; the GNU General Public License as published by
; the Free Software Foundation; version 3, or (at your option)
; any later version,
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with GNU Emacs; see the file COPYING. If not, see <http://www.gnu.org/licenses/>.
(require 'cal-french)
(defvar cal-french-check-num 0)
(defun ymd-to-mdy (date)
  "Convert the date from the logical format to the funny format"
  (let ((y (car date))
        (m (cadr date))
        (d (nth 2 date)))
        (list m d y))
)
(defun cal-french-check-unit (date expected-string)
  "Unit check for cal-french.el"
  (let ((actual-string (calendar-french-date-string (ymd-to-mdy date))))
         (setq cal-french-check-num (1+ cal-french-check-num))
         (cond ((equal expected-string actual-string)
                    (princ (format "ok %d
" cal-french-check-num)))
               (t
                    (princ (format "not ok %d got %s expected %s
" cal-french-check-num actual-string expected-string)))
         )
  )
)
(defun cal-french-check ()
   "Self-check for cal-french.el"
    (cal-french-check-unit '(1792  9 22) "Primidi 1 Vendémiaire an 1 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1793 10 23) "Duodi 2 Brumaire an 2 de la Révolution, jour du Céleri")
    (cal-french-check-unit '(1794  7 27) "Nonidi 9 Thermidor an 2 de la Révolution, jour de la Mûre")
    (cal-french-check-unit '(1794 11 23) "Tridi 3 Frimaire an 3 de la Révolution, jour de la Chicorée")
    (cal-french-check-unit '(1795 10  5) "Tridi 13 Vendémiaire an 4 de la Révolution, jour du Potiron")
    (cal-french-check-unit '(1795 12 25) "Quartidi 4 Nivôse an 4 de la Révolution, jour du Soufre")
    (cal-french-check-unit '(1797  1 24) "Quintidi 5 Pluviôse an 5 de la Révolution, jour du Taureau")
    (cal-french-check-unit '(1798  2 24) "Sextidi 6 Ventôse an 6 de la Révolution, jour de l'Asaret")
    (cal-french-check-unit '(1799 11  9) "Octidi 18 Brumaire an 8 de la Révolution, jour de la Dentelaire")
    (cal-french-check-unit '(1801  3 29) "Octidi 8 Germinal an 9 de la Révolution, jour de la Jonquille")
    (cal-french-check-unit '(1804  4 30) "Décadi 10 Floréal an 12 de la Révolution, jour du Rateau")
    (cal-french-check-unit '(1807  6  1) "Duodi 12 Prairial an 15 de la Révolution, jour de la Bétoine")
    (cal-french-check-unit '(1810  7  3) "Quartidi 14 Messidor an 18 de la Révolution, jour de la Lavande")
    (cal-french-check-unit '(1813  8  4) "Sextidi 16 Thermidor an 21 de la Révolution, jour de la Guimauve")
    (cal-french-check-unit '(1816  9  4) "Octidi 18 Fructidor an 24 de la Révolution, jour du Nerprun")
    (cal-french-check-unit '(2000  1  1) "Duodi 12 Nivôse an 208 de la Révolution, jour de l'Argile")
    (cal-french-check-unit '(2021  7 11) "Tridi 23 Messidor an 229 de la Révolution, jour des Haricots")
    (cal-french-check-unit '(2001  5 11) "Duodi 22 Floréal an 209 de la Révolution, jour de la Fritillaire")
    (cal-french-check-unit '(1792  9 22) "Primidi 1 Vendémiaire an 1 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1793  9 21) "Quintidi 5 jour complémentaire an 1 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1793  9 22) "Primidi 1 Vendémiaire an 2 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1794  9 21) "Quintidi 5 jour complémentaire an 2 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1794  9 22) "Primidi 1 Vendémiaire an 3 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1795  9 22) "Sextidi 6 jour complémentaire an 3 de la Révolution, jour de la Révolution")
    (cal-french-check-unit '(1795  9 23) "Primidi 1 Vendémiaire an 4 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1796  9 21) "Quintidi 5 jour complémentaire an 4 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1796  9 22) "Primidi 1 Vendémiaire an 5 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1797  9 21) "Quintidi 5 jour complémentaire an 5 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1797  9 22) "Primidi 1 Vendémiaire an 6 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1799  9 22) "Sextidi 6 jour complémentaire an 7 de la Révolution, jour de la Révolution")
    (cal-french-check-unit '(1799  9 23) "Primidi 1 Vendémiaire an 8 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1800  9 22) "Quintidi 5 jour complémentaire an 8 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1800  9 23) "Primidi 1 Vendémiaire an 9 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1801  9 22) "Quintidi 5 jour complémentaire an 9 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1801  9 23) "Primidi 1 Vendémiaire an 10 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1823  9 22) "Quintidi 5 jour complémentaire an 31 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1823  9 23) "Primidi 1 Vendémiaire an 32 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1824  9 22) "Sextidi 6 jour complémentaire an 32 de la Révolution, jour de la Révolution")
    (cal-french-check-unit '(1824  9 23) "Primidi 1 Vendémiaire an 33 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1825  9 22) "Quintidi 5 jour complémentaire an 33 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1825  9 23) "Primidi 1 Vendémiaire an 34 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1892  9 21) "Quintidi 5 jour complémentaire an 100 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1892  9 22) "Primidi 1 Vendémiaire an 101 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1900  9 22) "Sextidi 6 jour complémentaire an 108 de la Révolution, jour de la Révolution")
    (cal-french-check-unit '(1900  9 23) "Primidi 1 Vendémiaire an 109 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(1992  9 21) "Quintidi 5 jour complémentaire an 200 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(1992  9 22) "Primidi 1 Vendémiaire an 201 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(2000  9 21) "Sextidi 6 jour complémentaire an 208 de la Révolution, jour de la Révolution")
    (cal-french-check-unit '(2000  9 22) "Primidi 1 Vendémiaire an 209 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(2092  9 20) "Quintidi 5 jour complémentaire an 300 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(2092  9 21) "Primidi 1 Vendémiaire an 301 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(2100  9 21) "Sextidi 6 jour complémentaire an 308 de la Révolution, jour de la Révolution")
    (cal-french-check-unit '(2100  9 22) "Primidi 1 Vendémiaire an 309 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(2192  9 21) "Sextidi 6 jour complémentaire an 400 de la Révolution, jour de la Révolution")
    (cal-french-check-unit '(2192  9 22) "Primidi 1 Vendémiaire an 401 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(2193  9 21) "Quintidi 5 jour complémentaire an 401 de la Révolution, jour des Récompenses")
    (cal-french-check-unit '(2199  9 22) "Primidi 1 Vendémiaire an 408 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(2200  9 22) "Sextidi 6 jour complémentaire an 408 de la Révolution, jour de la Révolution")
    (cal-french-check-unit '(2791  9 23) "Primidi 1 Vendémiaire an 1000 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(2792  9 22) "Primidi 1 Vendémiaire an 1001 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(3000  1  1) "Duodi 12 Nivôse an 1208 de la Révolution, jour de l'Argile")
    (cal-french-check-unit '(3001  1  1) "Primidi 11 Nivôse an 1209 de la Révolution, jour du Granit")
    (cal-french-check-unit '(3791  9 22) "Primidi 1 Vendémiaire an 2000 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(3792  9 22) "Primidi 1 Vendémiaire an 2001 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(4000  1  1) "Duodi 12 Nivôse an 2208 de la Révolution, jour de l'Argile")
    (cal-french-check-unit '(4001  1  1) "Duodi 12 Nivôse an 2209 de la Révolution, jour de l'Argile")
    (cal-french-check-unit '(4320  9 10) "Quartidi 24 Fructidor an 2528 de la Révolution, jour du Sorgho")
    (cal-french-check-unit '(4320  9 11) "Quintidi 25 Fructidor an 2528 de la Révolution, jour de l'Écrevisse")
    (cal-french-check-unit '(4791  9 23) "Primidi 1 Vendémiaire an 3000 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(4792  9 22) "Primidi 1 Vendémiaire an 3001 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(5000  1  1) "Duodi 12 Nivôse an 3208 de la Révolution, jour de l'Argile")
    (cal-french-check-unit '(5001  1  1) "Primidi 11 Nivôse an 3209 de la Révolution, jour du Granit")
    (cal-french-check-unit '(5791  9 22) "Primidi 1 Vendémiaire an 4000 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(5792  9 21) "Primidi 1 Vendémiaire an 4001 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(6000  1  1) "Tridi 13 Nivôse an 4208 de la Révolution, jour de l'Ardoise")
    (cal-french-check-unit '(6001  1  1) "Tridi 13 Nivôse an 4209 de la Révolution, jour de l'Ardoise")
    (cal-french-check-unit '(6791  9 22) "Primidi 1 Vendémiaire an 5000 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(6792  9 21) "Primidi 1 Vendémiaire an 5001 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(7791  9 21) "Primidi 1 Vendémiaire an 6000 de la Révolution, jour du Raisin")
    (cal-french-check-unit '(7792  9 21) "Primidi 1 Vendémiaire an 6001 de la Révolution, jour du Raisin")
)
(message "%s" "c est parti
")
(cal-french-check)
