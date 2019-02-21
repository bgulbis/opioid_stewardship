SELECT
	ENCOUNTER.ENCNTR_ID AS ENCOUNTER_ID,
	CLINICAL_EVENT.EVENT_ID AS EVENT_ID,
	CV_ENCOUTER_TYPE.DISPLAY AS ENCOUNTER_TYPE,
	TO_CHAR(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS CLINICAL_EVENT_DATETIME,
	TO_CHAR(pi_from_gmt(CE_MED_RESULT.ADMIN_START_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS ADMIN_START_DATETIME,
	CV_EVENT.DISPLAY AS MED,
	CE_MED_RESULT.ADMIN_DOSAGE AS DOSE,
	CV_DOSAGE_UNIT.DISPLAY AS DOSE_UNIT,
	CE_MED_RESULT.INFUSION_RATE AS RATE,
	CV_RATE_UNIT.DISPLAY AS RATE_UNIT,
	CV_IV_EVENT.DISPLAY AS IV_EVENT,
	CV_ROUTE_CE.DISPLAY AS ROUTE_EVENT,
	CV_NURSE_UNIT.DISPLAY AS NURSE_UNIT_EVENT,
	CV_FACILITY.DISPLAY AS FACILITY_EVENT,
	CV_EVENT_ADMIN.DISPLAY AS ADMIN_EVENT,
	CASE ORDERS.PRN_IND
		WHEN 1 THEN 'TRUE'
		ELSE 'FALSE'
	END AS PRN_DOSE,
	CLINICAL_EVENT.ORDER_ID AS ORDER_ID,
	ORDERS.TEMPLATE_ORDER_ID AS TEMPLATE_ORDER_ID,
	CASE ORDERS.TEMPLATE_ORDER_ID
		WHEN 0 THEN CLINICAL_EVENT.ORDER_ID
		ELSE ORDERS.TEMPLATE_ORDER_ID
	END AS ORIG_ORDER_ID
FROM
	CE_MED_RESULT,
	CLINICAL_EVENT,
	CODE_VALUE CV_DOSAGE_UNIT,
	CODE_VALUE CV_ENCOUTER_TYPE,
	CODE_VALUE CV_EVENT,
	CODE_VALUE CV_EVENT_ADMIN,
	CODE_VALUE CV_FACILITY,
	CODE_VALUE CV_IV_EVENT,
	CODE_VALUE CV_NURSE_UNIT,
	CODE_VALUE CV_ORDER,
	CODE_VALUE CV_RATE_UNIT,
	CODE_VALUE CV_ROUTE_CE,
	DIAGNOSIS,
	ENCNTR_LOC_HIST,
	ENCOUNTER,
	MED_ADMIN_EVENT,
	NOMENCLATURE,
	ORDERS,
	PI_THERA_CLASS_VIEW
WHERE
    NOMENCLATURE.SOURCE_IDENTIFIER IN ('D57', 'D57.1', 'D57.2', 'D57.3', 'D57.4', 'D57.8')
    AND NOMENCLATURE.ACTIVE_IND = 1
    AND (
        NOMENCLATURE.NOMENCLATURE_ID = DIAGNOSIS.NOMENCLATURE_ID
        AND DIAGNOSIS.ACTIVE_IND = 1
    )
    AND PI_THERA_CLASS_VIEW.DRUG_CAT IN (
        'cox-2 inhibitors',
        'gamma-aminobutyric acid analogs',
        'miscellaneous analgesics',
        'narcotic analgesic combinations',
        'narcotic analgesics',
        'nonsteroidal anti-inflammatory agents'
	)
	AND PI_THERA_CLASS_VIEW.DRUG_CAT_CD = CV_ORDER.CODE_VALUE
	AND CV_ORDER.CODE_SET = 200
	AND CV_ORDER.DISPLAY_KEY = CV_EVENT.DISPLAY_KEY
	AND CV_EVENT.CODE_SET = 72
	AND CV_EVENT.CODE_VALUE = CLINICAL_EVENT.EVENT_CD
	AND DIAGNOSIS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND (
		CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
		AND ORDERS.ACTIVE_IND = 1
	)
	AND (
	    DIAGNOSIS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND ENCOUNTER.ACTIVE_IND = 1
		AND ENCOUNTER.LOC_FACILITY_CD IN (3310, 3796, 3821, 3822, 3823)
		AND ENCOUNTER.ENCNTR_TYPE_CD = CV_ENCOUTER_TYPE.CODE_VALUE
	)
	AND (
		CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
		AND CLINICAL_EVENT.EVENT_END_DT_TM >= ENCNTR_LOC_HIST.TRANSACTION_DT_TM
		AND (CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM)
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.ACTIVE_IND = 1
				AND CLINICAL_EVENT.EVENT_END_DT_TM >= ELH.TRANSACTION_DT_TM
		)
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = CV_NURSE_UNIT.CODE_VALUE
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD = CV_FACILITY.CODE_VALUE
	)
	AND (
		CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.ADMIN_ROUTE_CD = CV_ROUTE_CE.CODE_VALUE
		AND CE_MED_RESULT.DOSAGE_UNIT_CD = CV_DOSAGE_UNIT.CODE_VALUE
		AND CE_MED_RESULT.INFUSION_UNIT_CD = CV_RATE_UNIT.CODE_VALUE
		AND CE_MED_RESULT.IV_EVENT_CD = CV_IV_EVENT.CODE_VALUE
	)
	AND (
		CLINICAL_EVENT.EVENT_ID = MED_ADMIN_EVENT.EVENT_ID(+)
		AND MED_ADMIN_EVENT.EVENT_TYPE_CD = CV_EVENT_ADMIN.CODE_VALUE(+)
	)
	AND (
		ENCOUNTER.DISCH_DT_TM + 0
			BETWEEN DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', pi_to_gmt(TRUNC(SYSDATE), pi_time_zone(2, @Variable('BOUSER'))),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - 1, pi_time_zone(2, @Variable('BOUSER'))),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE - 1, 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, @Variable('BOUSER'))),
				'N Days Prior', pi_to_gmt(SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {'0'}, User:2080), pi_time_zone(2, @Variable('BOUSER')))
			)
			AND DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', pi_to_gmt(TRUNC(SYSDATE) + (86399 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE, 'DAY') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, @Variable('BOUSER'))),
				'N Days Prior', pi_to_gmt(SYSDATE, pi_time_zone(2, @Variable('BOUSER')))
			)
		AND ENCOUNTER.DISCH_DT_TM
			BETWEEN DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', TRUNC(SYSDATE),
				'Yesterday', TRUNC(SYSDATE) - 1,
				'Week to Date', TRUNC(SYSDATE, 'DAY'),
				'Last Week', TRUNC(SYSDATE - 7, 'DAY'),
				'Last Month', TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'),
				'Month to Date', TRUNC(SYSDATE - 1, 'MONTH'),
				'User-defined', DECODE(
					@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
					'01/01/1800 00:00:00',
					'',
					@Variable('Enter begin date (Leave as 01/01/1800 if using a Relative Date)')
				),
				'N Days Prior', SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {0}, User:2080)
			) - 1
			AND DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', TRUNC(SYSDATE) + (86399 / 86400),
				'Yesterday', TRUNC(SYSDATE) - (1 / 86400),
				'Week to Date', TRUNC(SYSDATE) - (1 / 86400),
				'Last Week', TRUNC(SYSDATE, 'DAY') - (1 / 86400),
				'Last Month', TRUNC(SYSDATE, 'MONTH') - (1 / 86400),
				'Month to Date', TRUNC(SYSDATE) - (1 / 86400),
				'User-defined', DECODE(
					@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
					'01/01/1800 00:00:00',
					'',
					@Variable('Enter end date (Leave as 01/01/1800 if using a Relative Date)')
				),
				'N Days Prior', SYSDATE
			) + 1
	)
