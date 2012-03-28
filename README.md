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

* ### External componenets ###
    * [__reaper__] (http://en.wikipedia.org/wiki/Reaper)
        - SOURCE: system command that generates output containing data we're
          interested in

    * [__combine__] (http://en.wikipedia.org/wiki/Combine_harvester)
        - SOURCE and CLIENT: any external component that gathers data and sends
          it to the reclaimer directly

    * [__sieve__] (http://en.wikipedia.org/wiki/Sieve)
        - PLUG-IN: lives with reclaimer(s), takes raw reaper output as input
          and outputs formatted, extracted data
        - Executed by the reclaimer on the appropriate raw output types

* ### Internal componenets ###
    * [__stacker__] (http://en.wikipedia.org/wiki/Stacker)
        - CLIENT: lives on target reaper machines and stacks raw outputs
        - Executed via cron
        - Executes data-collection (reaper), system commands, then compresses
          and stores their raw outputs in a queue directory (for later pick-up
          by the reclaimer), using filenames for meta-data
        - Optionally, pushes data to reclaimer via UDP

    * [__reclaimer__] (http://en.wikipedia.org/wiki/Reclaimer)
        - SERVER: lives on mother-ship machine(s) and sorts through stacked
          outputs, dispatching processing and delivery
        - Executed as a daemon
        - Connects to client machines and picks-up raw output files from the
          queue directory
        - Optionally, receives data directly from stackers via UDP
        - Dispatches processing of raw outputs through appropriate sieves
        - Delivers extracted data to final consumers for presentation and/or
          analysis (Graphite, Cacti, Munin, a data scientist, etc).
