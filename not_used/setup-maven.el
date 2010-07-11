(defvar mvn-command-history nil
 "Maven command history variable")

(defun mvn(&optional args)
 "Runs maven in the current project. Starting at the directoy
  where the file being vsisited resides, a search is made for
  pom.xml recsurively. A maven command is made from the first
  directory where the pom.xml file is found is then displayed in
  the minibuffer. The command can be edited as needed and then
  executed. Errors are navigate to as in any other compile mode"
  (interactive)
  (let ((fn (buffer-file-name)))
    (let ((dir (file-name-directory fn)))
      (while (and (not (file-exists-p (concat dir "/pom.xml")))
                (not (equal dir (file-truename (concat dir "/..")))))
        (setf dir (file-truename (concat dir "/.."))))
      (if (not (file-exists-p (concat dir "/pom.xml")))
        (message "No pom.xml found")
        (compile (read-from-minibuffer "Command: "
          (concat "mvn -o -f " dir "/pom.xml compile |sed -e 's/\\[INFO\\] //g' |sed -e 's/\\[WARNING\\] //g'") nil nil 'mvn-command-history))))))
          ;; for mac/unix
          ;(concat "mvn -f " dir "/pom.xml compile |sed -e s/\\\\[INFO\\\\][[:space:]]//g |sed -e s/\\\\[WARNING\\\\][[:space:]]//g") nil nil 'mvn-command-history))))))

