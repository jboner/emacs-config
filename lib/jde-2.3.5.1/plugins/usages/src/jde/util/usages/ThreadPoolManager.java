package jde.util.usages;

import java.util.List;
import java.util.Iterator;
import java.util.LinkedList;
import jde.util.Usages;

/**
 * Describe class ThreadPoolManager here.
 *
 *
 * Created: Fri Jul 30 17:42:22 2004
 *
 * @author <a href="mailto:sacharya@bea.com"></a>
 * @version 1.0
 */
public class ThreadPoolManager {

    List threads = null;
    List tasks = new LinkedList();
    int poolSize;
    volatile private boolean done = false;
    
    public ThreadPoolManager(int poolSize) {
        this.poolSize = poolSize;
    }

    public void submit (Runnable r) {
        synchronized (tasks) {
            tasks.add (r);
            tasks.notify();
        }
    }


    public synchronized void start () {
        if (threads != null)
            return;
        
        threads = new LinkedList ();
        for (int i = 0; i < poolSize;i++) {
            Thread t = new Thread() {
                    public void run () {
                        
                        while (true) {

                            Runnable task = null;
                            synchronized (tasks) {
                                while (tasks.size() == 0 && !done)
                                    try {tasks.wait();} catch (InterruptedException e) { Usages.message (e);}
                                if (tasks.size() != 0)
                                    task = (Runnable) tasks.remove(0);
                                else if (done)
                                    break;
                            }
                            if (task != null) {
                                task.run();
                                task = null;
                            }
                        }
                        
                    }
                };
            t.start();
            threads.add (t);
        }
    }


    public void done () {
        synchronized (tasks) {
            done = true;
            tasks.notifyAll();
        }
    }

    public synchronized void waitTillAllDone () {
        start();
        done();
        for (Iterator it = threads.iterator();it.hasNext();) {
            Thread t = (Thread) it.next();
            try {t.join(0);} catch (InterruptedException e)   { Usages.message (e);}
        }
    }

}
