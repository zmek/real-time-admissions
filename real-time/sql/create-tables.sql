create table flow.ed_predictor_csn (
  prob1 FLOAT(8),
  prob0 FLOAT(8),
  timeslice VARCHAR(7),
  csn VARCHAR(10),
  extract_dttm timestamptz
  )
;

alter table flow.ed_predictor_agg_audit add description varchar(30);

select * from flow.ed_predictor_agg;

drop table flow.ed_predictor_pred_stats

 alter table flow.ed_predictor_csn_audit rename column time_since_arrival to time_since_arrival_old;

alter table flow.ed_predictor_csn_audit add time_since_arrival int;

select * from flow.ed_predictor_csn_audit where extract_dttm =  (select max( extract_dttm)  from flow.ed_predictor_csn_audit)