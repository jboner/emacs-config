(jde-project-file-version "1.0")
(jde-set-variables
 '(jde-project-name "jde-usages-tests")
 '(jde-global-classpath (quote ("./classes" "./lib/testproject.jar")))
 '(jde-compile-option-directory "./classes")
 '(jde-sourcepath (quote ("./src" "../src"))))
(custom-set-variables
  '(bsh-vm-args (quote ("-Xdebug" "-Xnoagent" "-Djava.compiler=NONE" "-Xrunjdwp:transport=dt_shmem,server=y,suspend=n,address=javadebug"))))
