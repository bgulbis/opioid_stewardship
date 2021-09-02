SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
	pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS DOSE_DATETIME,
	CLINICAL_EVENT.EVENT_ID,
	CE_MED_RESULT.ADMIN_DOSAGE AS DOSE,
	pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSE_UNIT,
	-- CE_MED_RESULT.ADMIN_ROUTE_CD,
	pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ROUTE,
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
FROM
	CE_MED_RESULT,
	CLINICAL_EVENT,
	ENCNTR_ALIAS,
	ENCNTR_LOC_HIST
WHERE
	CLINICAL_EVENT.EVENT_CD = 37557647 -- naloxone
	AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
		DECODE(
			@Prompt('Choose date range', 'A', {'Last Month', 'User-defined'}, mono, free, , , User:0),
			'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), 'America/Chicago'),
			'User-defined', pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'07/01/2021 00:00:00'}, User:1),
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				),
				pi_time_zone(1, 'America/Chicago')
			)
		)
		AND DECODE(
			@Prompt('Choose date range', 'A', {'Last Month', 'User-defined'}, mono, free, , , User:0),
			'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - 1/86400, 'America/Chicago'),
			'User-defined', pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'08/01/2021 00:00:00'}, User:2),
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400,
				pi_time_zone(1, 'America/Chicago')
			)
		)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
	AND CE_MED_RESULT.ADMIN_DOSAGE > 0
	AND CE_MED_RESULT.ADMIN_ROUTE_CD IN (
		508984, -- IV
		9022649, -- IVP
		9022647, -- IVPB
		354081141, -- IV Central
		509008, -- SUB-Q
		508982 -- IM
	)
	AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
	AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
	AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
		SELECT MAX(ELH.TRANSACTION_DT_TM)
		FROM ENCNTR_LOC_HIST ELH
		WHERE
			CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
			AND ELH.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
	)
	AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
	AND ENCNTR_LOC_HIST.LOC_FACILITY_CD IN (
		3310, -- HH HERMANN
		3796, -- HC Childrens
		3821, -- HH Clinics
		3822, -- HH Trans Care
		3823 -- HH Rehab
	)
	AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1
