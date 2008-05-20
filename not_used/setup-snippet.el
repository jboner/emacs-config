;; ================================================
(load-file (substitute-in-file-name "$EMACS_LIB/lib/misc/smart-snippet.el"))
(require 'smart-snippet)
(setq snippet-skip-same-field t)

;; ================================================
;; note TAB can be different to <tab> in X mode(not -nw mode).
;; the formal is C-i while the latter is the real "Tab" key in
;; your keyboard.
;(define-key scala-mode-map (kbd "TAB") 'kid-c-escape-pair)
;;;(define-key scala-mode-map (kbd "<control> <tab>") 'kid-c-escape-pair)

;; snippet.el use TAB, now we need to use <tab>
;(define-key snippet-map  (kbd "<tab>") 'snippet-next-field)

;; ================================================
;; Scala snippets
(smart-snippet-with-abbrev-tables 
  (scala-mode-abbrev-table)
  ("same" "if ($${test}, $${test})\n{$]\n$]$.\n$]}" 'bol?)

  ; imports etc
  ("pac" "package $." 'bol?)
  ("imp" "import $." 'bol?)

  ; conditional
  ("if" "if ($${condition}) {$]\n$]$.\n$]}" 'bol?)
  ("ife" "if ($${true}) {\n$>$.\n$>} else  {\n$>\n$>}" 'bol?)

  ; loops and for comprehensions
  ("forc" "for ($${element} <- $${iterable}) {\n$>yield $.\n}" 'bol?)
  ("forif" "for ($${element} <- $${iterable} if $${guard}) {\n$>yield $.\n$>}" 'bol?)
  ("forl" "for ($${element} <- $${iterable}) {\n$>$.\n$>}" 'bol?)
  ("fora" "$${element} <- $${iterable})" '(not in-comment?)) ; extract var in for comprehension
  ("whi" "while ($${true}) {\n$>$.\n$>}" 'bol?)

  ; var and val
  ("val" "val $${name} = $." 'bol?)
  ("var" "var $${name} = $${_}$." 'bol?)
  ("nval" "val $${name} = new $${type}($${args})" 'bol?)
  ("nvar" "var $${name} = new $${type}($${args})" 'bol?)

  ; functions 
  ; Syntax fXYZ => X (has args?): Y (has return type?): Z (is one liner?)
  ("main" "def main(args: Array[String]) {\n$>$.\n}" 'bol?)
  ("f000" "def $${name} = $." 'bol?)
  ("f001" "def $${name} = $.{\n$>$.\n}" 'bol?)
  ("f010" "def $${name}: $${Unit} = $." 'bol?)
  ("f100" "def $${name}($${args}) = $." 'bol?)
  ("f110" "def $${name}($${args}): $${Unit} = $." 'bol?)
  ("f011" "def $${name}: $${Unit} = {\n$>$.\n}" 'bol?)
  ("f101" "def $${name}($${args}) = {\n$>$.\n}" 'bol?)
  ("f111" "def $${name}($${args}): $${Unit} = {\n$>$.\n}" 'bol?)
  ("ano" "($${args}) => $${body}$." 'bol?)

  ; tracing
  ("prn" "println(\"$${message}\") $." 'bol?)
  ("pr" "print(\"$${message}\") $." 'bol?)

  ; classes etc.
  ("mix" "trait $${name} {\n$>$.\n}" 'bol?) 
  ("cl" "class $${name} {\n$>$.\n$>}" 'bol?)
  ("cla" "class $${name}($${args}) {\n$>$.\n$>}" 'bol?)
  ("acl" "abstract class $${name} {\n$>$.\n$>}" 'bol?)
  ("acla" "abstract class $${name}($${name}: $${type}) {\n$>$.\n$>}" 'bol?)
  ("ccl" "case class $${name}($${name}: $${type})$." 'bol?)
  ("cob" "case object $${name} $." 'bol?)
  ("ext" "extends $${name} $." '(not in-comment?))
  ("with" "with $${trait} $." '(not in-comment?))
  ("ob" "objcet $${name} extends $${type}$." '(not in-comment?))
  
  ; modifiers
  ("pri" "private[$${scope}] $." 'bol?)
  ("pro" "protected[$${scope}] $." 'bol?)

  ; data structures
  ("nmap" "val $${name} = new HashMap[$${key}, $${value}]" 'bol?)  
  ("nset" "val $${name} = new HashSet[$${value}]" 'bol?)  
  ("narr" "new Array($${type})($${arg})" '(not in-comment?))
  ("nlis" "List($${v1}, $${v2}$.)" '(not in-comment?))
  ("tup" "($${v1}, $${v2}$.)" '(not in-comment?))
  ("cons" " :: $." '(not in-comment?))
  ("conc" " ::: $." '(not in-comment?))

  ; actors
  ("dact" "class $${name} extends Actor {\n$>def act = {\n$>loop {\n$>react {\n$>$.\n$>}\n$>}\n$>}\n}" 'bol?)
  ("act" "def act = {\n$>loop {\n$>react {\n$>$.\n$>}\n$>}\n$>}" 'bol?)
  ("acta" "def act = {\n$>loop($${arg})\n$>}\n$>}\n\n$>def loop($${arg}: $${type}) {\n$>react {\n$>$.\n$>}\n$>}" 'bol?)
  ("bang" "$${client} ! $${message} $." 'bol?)
  ("trap" "self.trapExit = true" 'bol?)

  ; matching
  ("mat" "$${arg} match {\n$>case $${pattern} => \n$>$${statement}\n$>case $${_} => \n$>$.\n}" '(not in-comment?))
  ("tc" "try {\n$>$.\n} catch {\n$>case e: $${Exception} => $${println(\"ERROR: \" + e) // TODO: handle exception}\n} $." 'bol?)
  ("tf" "try {\n$>$.\n} finally {\n$.\n}" 'bol?)
  ("tcf" "try {\n$>$.\n} catch {\n$>case e: $${Exception} => $${println(\"ERROR: \" + e) // TODO: handle exception}\n} finally {\n$.\n}" 'bol?)
  ("mex" "e: $${Exception} $." 'bol?)
  ("tex" "throw new $${Exception}() $." 'bol?)
  ("ca_" "case $${_} => \n$>$." 'bol?)
  ("ca" "case $${_} => $." 'bol?)
  ;("ca" "case $${class}($${name}: $${type}) =>\n$>$." 'bol?)
  ("isof" "isInstanceOf[$${type}]$." 'bol?)
  ("asof" "asInstanceOf[$${type}]$." 'bol?)
  ("clof" "classOf[$${type}]$." 'bol?)

  ; testing
  ("suite" "import org.scalatest._\n\nclass $${name} extends Suite {\n$>$.\n$>}" 'bol?)
  ("test" "def test$${name} = {\n$>$.\n}" 'bol?)
  ("asst" "assert($${true})" 'bol?)
  ("ass" "assert($${value} === $${expected})" 'bol?)
  ("expect" "expect($${value}) {\n$>$.\n}" 'bol?)
  ("intercept" "intercept(classOf[$${Exception}]) {\n$>$.\n}" 'bol?)
  
  ;; TODO: map filter zip flatten reduceLeft reduceRight foldLeft foldRight reverse
  ; list functions
  ("lfor" "foreach(x => $${body}))" '(not in-comment?))
  ("lmap" "map($${args} => $${body})" '(not in-comment?))
)


;; ================================================
;; Common templates
;
;; those non-word snippet can't be triggered by abbrev expand, we
;; need to bind them explicitly to some key
(smart-snippet-with-abbrev-tables
  (scala-mode-abbrev-table
   java-mode-abbrev-table)
   ("{" "{$.}" '(not (c-in-literal)))
   ("{" "{$>\n$>$.\n}$>" 'bol?)
   ;; if not in comment or other stuff(see `c-in-literal'), then
   ;; inser a pair of quote. if already in string, insert `\"'
   ("\"" "\"$.\"" '(not (c-in-literal)))	
   ("\"" "\\\"$." '(eq (c-in-literal) 'string))
   ;; insert a pair of parenthesis, useful everywhere
   ("(" "($.)" t)
   ;; insert a pair of angular bracket if we are writing templates
   ("<" "<$.>" '(and (not (c-in-literal))
 			 (looking-back "template[[:blank:]]*")))
   ;; a pair of square bracket, also useful everywhere
   ("[" "[$.]" t)
   ;; a pair of single quote, if not in literal
   ("'" "'$.'" '(not (c-in-literal)))

   )

;; ================================================
;; Common templates
(smart-snippet-with-keymaps
  ((scala-mode-map scala-mode-abbrev-table)
   (java-mode-map java-mode-abbrev-table))
   ("{" "{")
   ("\"" "\"")
   ("(" "(")
   ("<" "<")
   ("[" "[")
   ("'" "'")
   )

;; ================================================
;; Jump out from a pair(like quote, parenthesis, etc.)
(defun kid-c-escape-pair ()
   (interactive)
   (let ((pair-regexp "[^])}\"'>]*[])}\"'>]"))
     (if (looking-at pair-regexp)
 	(progn
 	  ;; be sure we can use C-u C-@ to jump back
 	  ;; if we goto the wrong place
 	  (push-mark) 
 	  (goto-char (match-end 0)))
       (c-indent-command))))

;; ================================================
;; Java
(smart-snippet-with-abbrev-tables
  (java-mode-abbrev-table)
   ("if" "if ($${condition})\n{$>\n$>$.\n}$>" 'bol?)
   ("elsif" "else if ($${condition})\n{$>\n$>$.\n}$>" 'bol?)
   ("else" "else\n{$>\n$>$.\n}$>" 'bol?)
   ("for" "for ($${init}; $${cond}; $${step})\n{$>\n$>$.\n}$>" 'bol?)
   ("namespace" "namespace $${name} {\n$.\n} // namespace $${name}" 'bol?))
    
;(snippet-with-abbrev-table 'java-mode-abbrev-table
;  ("if"  .  "if ($${true}) {\n$>$.\n}")
;  ("ife"  .  "if ($${true}) {\n$>$.\n} else  {\n$>\n}")
;  ("elif"  .  "else  {\n$>$${// TODO: add statement}\n} if ($${true}) {\n$>$.\n} ")
;  ("else"  .  "else  {\n$>$${// TODO: add statement}\n}$.")
;  ("for" .  "for (int $${i} = 0; $${i} < $${range}; $${i}++) {\n$>$.\n}")
;  ("fore" .  "for ($${iterable_type} $${iterable_element} : $${iterable}) {\n$>$.\n}")
;  ("itar" .  "for (int $${index} = 0; $${index} < $${array}.length; $${index}++) {\n$>$.\n}")
;  ("itco" .  "for (Iterator $${iterator} = $${collection}.iterator(); $${iterator}.hasNext(); {\n$>$${type} $${element} = ($${type}) $${iterator}.next();\n$>$.\n}")
;  ("while" .  "while ($${true}) {\n$>$.\n}")
;  ("whileen" .  "while ($${enumeration}.hasMoreElements()) {\n$>$${type} $${element} = ($${type}) $${enumeration}.nextElement();\n$>$.\n}")
;  ("whileit" .  "while ($${iterator}.hasNext()) {\n$>$${type} $${element} = ($${type}) $${iterator}.next();\n$>$.\n}")
;  ("do"  .  "do {\n$>$.\n} while ($${True});")
;  ("lazy" .  "if ($${name} == null) {\n$>$${name} = new $${type}($${arguments});\n$>$.\n}\nreturn $${name};")
;  ("new" .  "$${type} $${name} = new $${type}($${arguments});")
;  ("try" .  "try {\n$>$.\n} catch ($${Exception} e) {\n$>$${e.printStackTrace(); // TODO: handle exception}\n}")
;  ("catch" .  "catch ($${Exception} e) {\n$>$${e.printStackTrace(); // TODO: handle exception}\n}\n$>$.")
;  ("cast" .  "$${type} $${new_name} = ($${type}) $${name};$.")
;  ("main" .  "public static void main(String[] args) {\n$>$.\n}")
;  ("defpri" .  "private $${return_type} $${name}($${args}) {\n$>$.\n}")
;  ("defpub" .  "public $${return_type} $${name}($${args}) {\n$>$.\n}")
;  ("defpro" .  "protected $${return_type} $${name}($${args}) {\n$>$.\n}")
;  ("defstat" .  "public static $${return_type} $${name}($${args}) {\n$>$.\n}")
;  ("runnable" .  "new Runnable() {\n$>public void run() {\n$>$.\n}")
;  ("switch" .  "switch ($${key}) {\n$>case $${value}:$.\n$>break;\n\n$>default:\n$>break;\n}")
;  ("sync" .  "synchronized ($${mutex}) {\n$>$.\n}")
;  ("serr" .  "System.err.println(\"$${ERROR: }\");$.")
;  ("sout" .  "System.out.println(\"$${message}\");$.")
;  ("strace" .  "System.out.println(\"$${var}:\" + $${var};$.")
;  ("test" .  "public void test$${name}() throws Exception {\n$>$.\n}")
;  ("intf" .  "public interface $${name} {\n$>$.\n}")
;  ("class" .  "public class $${name} {\n$>$.\n}")
;  ("iclass" .  "public static class $${name} {\n$>$.\n}")
;  ("extends" .  "extends $${name} $.")
;  ("const" .  "public static final $${type} $${name} = $.;")
;  ("toarr" .  "$${type}[] $${name} = ($${type}[]) $${collection}.toArray(new $${type}[$${collection}.size()])")
;  )

;; ================================================
;; Copied from Emacs 22.x distribution
(defun looking-back (regexp &optional limit greedy)
  "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.

If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
  (let ((start (point))
        (pos
         (save-excursion
           (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                (point)))))
    (if (and greedy pos)
        (save-restriction
          (narrow-to-region (point-min) start)
          (while (and (> pos (point-min))
                      (save-excursion
                        (goto-char pos)
                        (backward-char 1)
                        (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
            (setq pos (1- pos)))
          (save-excursion
            (goto-char pos)
            (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
    (not (null pos))))
