webzmachine
===========

Zotonic's Variation
-------------------

This is a fork of Basho's Webmachine.  This fork is done for supporting content management systems with large amounts of dispatch rules and many virtual hosts.


Differences with Basho Webmachine
---------------------------------

The main differences with Basho's Webmachine are:

* Pluggable dispatch handler
* Support for the HTTP Upgrade
* Caching of resource callbacks results
* Dispatch handler can redirect requests
* Use of process dictionary has been removed
* webmachine_request is now a normal (non parametrized) module
* Resources are renamed to controllers.
* webmachine_controller (ie. webmachine_resource) is now a non-parametrized module
* Extra logging, also in the event of crashes
* Use of os:timestamp() instead of now() to prevent locking and time shifts

The first couple of changes gave a significant speed boost to Webmachine.

In the specific case of Zotonic the difference was 5 milliseconds (or more) per request (on a 2GHz Core 2 Duo). Without these optimizations we were not able to use Webmachine.

