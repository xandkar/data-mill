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

A framework to organize automated data collection-and-processing pipelines.


Architectural Overview
----------------------

* ### External componenets ###
    * [__reaper__] (http://en.wikipedia.org/wiki/Reaper)
        - SOURCE: system command that generates output containing data we're
          interested in

    * [__combine__] (http://en.wikipedia.org/wiki/Combine_harvester)
        - SOURCE and CLIENT: any external component that gathers data and sends
          it to the SERVER directly

    * [__sieve__] (http://en.wikipedia.org/wiki/Sieve)
        - PLUGIN: lives with SERVER(s), takes raw SOURCE output as input
          and outputs formatted, extracted data
        - Executed by the SERVER on the appropriate raw output types

* ### Internal componenets ###
    * [__stacker__] (http://en.wikipedia.org/wiki/Stacker)
        - CLIENT: lives on target SOURCE machines and stacks raw outputs
        - Executed via cron or as a daemon (not yet decided)
        - Executes data-collection commands, then compresses and pushes their
          outputs to the SERVER via SSH or UDP (depending on sensitivity of
          data), using filenames for meta-data.

    * [__reclaimer__] (http://en.wikipedia.org/wiki/Reclaimer)
        - SERVER: lives on mother-ship machine(s) and sorts through stacked
          outputs, dispatching processing and delivery
        - Executed as a daemon
        - Receives raw output files from CLIENT (SSH or UDP)
        - Dispatches processing of raw outputs through appropriate PLUGINs
        - Delivers extracted data to final consumers for presentation and/or
          analysis (Graphite, Cacti, Munin, a data scientist, etc).
