"""This script implements a simple editor (based on EdSim) that
communicates with VoiceCode through a TCP/IP messaging protocol.
"""
import vc_globals

import sys
import util
from tcp_client import UneventfulLoop
from debug import config_traces, trace


config_traces(status="on",
              active_traces={
#                             'get_mess':1,
#                             'send_mess': 1,
#                             'SourceBuffEdSim': 1
                             })

#config_traces(status="on", active_traces='all')


def help():
    print """
Usage: python client_sim.py -h

or

python client_sim.py [OPTIONS]

where OPTIONS are 

[-m] [-p] [-i] [--host host] [--listen listen_port] [--talk talk_port]

runs an EdSim editor simulator as a TCP client to the mediator server, using
UneventfulLoop

OPTIONS
-------

-h :

   print this help message.

-m :
   allow EdSim to have multiple buffers

-p :
   have EdSim print the buffer whenever it changes

-i :
   use client-side indentation

--host host:
  specify the host name or IP address (Defaults to the local host)

--talk talk_port:
  specify the port number to use for the talk connection
    
--listen listen_port:
  specify the port number to use for the listen connection
    """
def run(multiple = 0, print_buff = 0, client_indentation = 0,
        host = None, listen_port = None, talk_port = None):
    l = UneventfulLoop(multiple, print_buff, 
        client_indentation = client_indentation)
    l.run(host, listen_port, talk_port)
    l.cleanup()



if __name__ == '__main__':


    opts, args = util.gopt(['h', None, 'm', None, 'p', None,
                            'i', None,
                            'host=', None,
                            'talk=', None, 'listen=', None])
    if opts['h']:
        help()
    else:
#        print sys.modules
        host = opts['host']
        listen_port = opts['listen']
        talk_port = opts['talk']
        multiple = 0
        if opts['m']:
            multiple = 1
        print_buff = 0
        if opts['p']:
            print_buff = 1
        client_indentation = 0
        if opts['i']:
            client_indentation = 1
        run(multiple, print_buff, client_indentation, host, 
            listen_port, talk_port)

