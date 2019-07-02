# PMP - PMP Monitors Production

## Synopsis

PMP aims at automating basic monitoring and investigation / troubleshooting tasks around the FAST production.

It works on par with the `manager` which is responsible for scheduling tasks (BOD snapshots for instance) and logging about those tasks.

## General idea

```
+----------------------------------------------------------------+
|                                                                |
|                                                                |
|                                                                |
|  +------------------------------------------------+            |    +-----------------------------------------------------+
|  |                                                |            |    |                                                     |
|  |                      ELK                       +<--------+  |    |                 PMP                                 |
|  |                                                |         |  |    |                                                     |
|  +-----------+-----------+-----------+------------+         |  |    +-----------------------------------------------------+
|  |           |           |           |            |         |  |    |                                                     |
|  |           |           |           |            |         |  |    |  API                                                |
|  | Elastic   | Logstash  | Filebeat  | Kibana     |         |  |    |                                                     |
|  |           |           |           |            |         |  |    |                                                     |
|  | (db and   | (parses   | (fetches  | (builds    |         |  +---------------->   Analyze  +----------------------+     |
|  | indexing/ | and sends | the logs  | dashboards |         |       |                                               |     |
|  | search    | the logs) | and feeds | based on   |         |       |                                               |     |
|  | engine)   |           | them to   | the data   |         |       |                                               |     |
|  |           |           | logstash  | logstash   |         |       |                                               |     |
|  |           |           | real-time)| generates) |         +------------------------------+                        |     |
+--+           |           |           |            |                 |                      |                        |     |
   |           |           |           |            |                 |                      |                        |     |
   |           |           |           |            +-----+           |                      +                        |     |
   +-+------+--+---+---+---+---+-----+-+---------+--+     |           |        Supervise ELK config                   |     |
     ^      ^      |   ^       |     ^           |        |           |        (log paths for logstash                |     |
     |      |      |   |       |     |           |        |           |        and filebeat, etc)                     |     |
     |      |      |   |       |     |           |        |           |                                               |     |
     |      |      |   |       |     |           |        |           |                      ^                        |     |
     |      +------+   +-------+     |           |        |           |                      |                        |     |
     |       Insert       Feed       |           |        |           |                      |                        |     |
     |                               |           |        |           |                      |                        |     |
     |                               |           |        |           |                      |                        |     |
     |                               |           |        |           |                      |                        |     |
     |                               |           |        |           |                      |                        |     |
     |                               | Fetch     |        |           |                      |                        |     |
     |                               |           |        |           +-----------------------------------------------------+
     |                               |           |        |           |                      |                        |     |
     |                               |           |        |           |  Webapp              |                        |     |
     |                               |           |        |           |                      |                        |     |
     |                               |           |        |           |                      |                        |     |
     |                               |           |        |           |                      |        Display  <------+     |
     |                               |           |        |           |                      |                              |
     |                               |           |        |           |                      |           ^                  |
     |                               |           |        |           |                      |           |                  |
     |                 +-------------+---+       |        |           |                      |           |                  |
     |                 |\\dfs            |       |        |           |                      |           |                  |
     |                 |                 |       |        |           |                      |           |                  |
     |                 |contains the FAST|       |        |           |               Define +           |                  |
     |                 |and manager logs |       |        |           |                                  |                  |
     |                 +-----------------+       |        |           |                                  |                  |
     |                                           |        |           |                                  |                  |
     |                                           |        +----------------------------------------------+                  |
     +-------------------------------------------+                    | Use custom autobuilt dashboards                     |
            Build dashboards from the data                            |                                                     |
                                                                      +-----------------------------------------------------+
```
