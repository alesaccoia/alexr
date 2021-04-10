WITH POI_GEOMETRIES_AS_POLYGONS AS (
  SELECT __PLACES_ID_COLUMN_NAME__ as id, st_geometryfromtext(__PLACES_WKT_COLUMN_NAME__)  AS geo_shape
  FROM __PLACES_TABLE_NAME__
  __PLACES_OPTIONS__
),
POI_DEVICES AS (
  SELECT ds, aid
  FROM emr.dataset_accumulo as device_pings, POI_GEOMETRIES_AS_POLYGONS areas
  WHERE ST_CONTAINS (areas.geo_shape, ST_POINT(device_pings.lon, device_pings.lat))
  AND ds between date '__DS_FROM__' and date '__DS_TO__' and aid is not null and aid != ''
  __AID_OPTIONS__
  group by ds, aid
),
POI_DEVICES_TIMES AS (
  SELECT areas.id, device_pings.ds, device_pings.source_name, device_pings.aid, arrival_date,
  if (ST_CONTAINS (areas.geo_shape, ST_POINT(device_pings.lon, device_pings.lat)), 1, 0) current_status
  FROM emr.dataset_accumulo as device_pings
  INNER JOIN POI_DEVICES b on device_pings.aid = b.aid and device_pings.ds = b.ds
  CROSS JOIN POI_GEOMETRIES_AS_POLYGONS areas
  where device_pings.ds between date '__DS_FROM__' and date '__DS_TO__'
),
FULL_TABLE AS (
  select source_name, id, ds, aid, arrival_date, current_status,
  ROW_NUMBER() OVER(partition by source_name, id, ds, aid Order By arrival_date) row_number_arrival_date
  from POI_DEVICES_TIMES
  GROUP BY source_name, id, ds, aid, arrival_date, current_status
  order by source_name, id, aid, ds, arrival_date
),
FULL_TABLE_ADORNED AS (
  select *,
  row_number_arrival_date - row_number() OVER (partition by id, aid Order By arrival_date) as grp
  from FULL_TABLE
  WHERE current_status = 1
  order by aid, arrival_date
)
select source_name, id, ds, aid, min(arrival_date) as min_time, max(arrival_date) as max_time, max(arrival_date) - min(arrival_date) as dwell_time
from FULL_TABLE_ADORNED
group by source_name,id,ds, aid, grp
order by source_name,id,ds, aid, min(arrival_date)
