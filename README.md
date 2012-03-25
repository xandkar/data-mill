Data Mill
=========

> _These ... machines...perform every necessary movement of the grain, and
> meal, from one part of the mill to another, and from one machine to another,
> through all the various operations, from the time the grain is emptied from
> the wagoner's bag....until completely manufactured into flour...without the
> aid of manual labor, excepting to set the different machines in motion._

-- [Oliver Evans] (http://en.wikipedia.org/wiki/Oliver_Evans),
inventor of the modern science of [handling materials] (
http://en.wikipedia.org/wiki/Bulk_material_handling).


Description
-----------

A system to organize automated data collection-and-processing pipelines.


Architectural Overview
----------------------

* [dmill_stacker] (http://en.wikipedia.org/wiki/Stacker)
    - CLIENT: lives on target sensor machines and stacks data
    - Executed via cron
    - Executes data-collection commands, compresses and stores their raw
      outputs in a queue directory (for later pick-up by the reclaimer), using
      filenames for meta-data;
    - Optionally, pushes data to reclaimer via UDP

* [dmill_reclaimer] (http://en.wikipedia.org/wiki/Reclaimer)
    - SERVER: lives on mother-ship machine(s) and sorts stacked data
    - Executed as a daemon
    - Connects to client machines and picks-up data files from the queue
      directory
    - Optionally, receives data directly from stackers via UDP
    - Dispatches filtering of raw data through appropriate sieves
    - Forwards clean data to final consumers for presentation and/or analysis
      (Graphite, Cacti, Munin, a data scientist, etc).

* __sieve__
    - PLUG-IN: lives with dmill_reclaimer and extracts target data out of raw
      sensor outputs
    - Executed by the dmill_reclaimer on the appropriate raw output types
    - stdin: raw output
    - stdout: formatted data
