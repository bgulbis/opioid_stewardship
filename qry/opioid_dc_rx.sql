SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
	ORDERS.ORDER_ID AS ORDER_ID,
	pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS ORDER_DATETIME,
	ORDER_CATALOG.PRIMARY_MNEMONIC AS MEDICATION,
	TO_NUMBER(OD_STRENGTHDOSE.OE_FIELD_DISPLAY_VALUE) AS DOSE,
	OD_STRENGTHDOSEUNIT.OE_FIELD_DISPLAY_VALUE AS DOSE_UNIT,
	TO_NUMBER(OD_VOLUMEDOSE.OE_FIELD_DISPLAY_VALUE) AS VOLUME,
	OD_VOLUMEDOSEUNIT.OE_FIELD_DISPLAY_VALUE AS VOLUME_UNIT,
	OD_FREQ.OE_FIELD_DISPLAY_VALUE AS FREQ,
	CASE ORDERS.PRN_IND
	    WHEN 1 THEN 'PRN'
	    ELSE 'Scheduled'
	END AS PRN,
	OD_PRNINSTRUCTIONS.OE_FIELD_DISPLAY_VALUE AS PRN_INSTRUCTIONS,
	TO_NUMBER(OD_DISPENSEQTY.OE_FIELD_DISPLAY_VALUE) AS DISPENSE_QTY,
	OD_DISPENSEQTYUNIT.OE_FIELD_DISPLAY_VALUE AS DISPENSE_QTY_UNIT,
	TO_NUMBER(OD_DURATION.OE_FIELD_DISPLAY_VALUE) AS DURATION,
	OD_DURATIONUNIT.OE_FIELD_DISPLAY_VALUE AS DURATION_UNIT,
	TO_NUMBER(OD_NBRREFILLS.OE_FIELD_DISPLAY_VALUE) AS NUM_REFILLS,
	ORDERS.ORDERED_AS_MNEMONIC AS ORDERED_AS,
	CV_ENCOUTER_TYPE.DISPLAY AS ENCOUNTER_TYPE,
    CV_NURSE_UNIT.DISPLAY AS NURSE_UNIT,
	PRSNL.NAME_FULL_FORMATTED AS PROVIDER,
	CV_POSITION.DISPLAY AS PROVIDER_POSITION
FROM
	CODE_VALUE CV_ENCOUTER_TYPE,
	CODE_VALUE CV_NURSE_UNIT,
	CODE_VALUE CV_POSITION,
	ENCNTR_ALIAS,
	ENCNTR_LOC_HIST,
	ENCOUNTER,
	ORDER_ACTION,
	ORDER_CATALOG,
	ORDER_DETAIL OD_DISPENSEQTY,
	ORDER_DETAIL OD_DISPENSEQTYUNIT,
	ORDER_DETAIL OD_DURATION,
	ORDER_DETAIL OD_DURATIONUNIT,
	ORDER_DETAIL OD_FREQ,
	ORDER_DETAIL OD_NBRREFILLS,
	ORDER_DETAIL OD_PRNINSTRUCTIONS,
	ORDER_DETAIL OD_STRENGTHDOSE,
	ORDER_DETAIL OD_STRENGTHDOSEUNIT,
	ORDER_DETAIL OD_VOLUMEDOSE,
	ORDER_DETAIL OD_VOLUMEDOSEUNIT,
	ORDERS,
	PI_THERA_CLASS_VIEW,
	PRSNL
WHERE
	PI_THERA_CLASS_VIEW.DRUG_CAT IN ('narcotic analgesic combinations', 'narcotic analgesics')
	AND (
		PI_THERA_CLASS_VIEW.DRUG_CAT_CD = ORDERS.CATALOG_CD
		AND ORDERS.ACTIVE_IND = 1
		AND ORDERS.CATALOG_TYPE_CD = 1363
		AND ORDERS.ORIG_ORD_AS_FLAG = 1
		AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)
	)
	AND PI_THERA_CLASS_VIEW.DRUG_CAT_CD = ORDER_CATALOG.CATALOG_CD
	AND (
		ORDERS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND ENCOUNTER.ACTIVE_IND = 1
		AND ENCOUNTER.LOC_FACILITY_CD IN (3310, 3796, 3821, 3822, 3823)
		AND ENCOUNTER.ENCNTR_TYPE_CD = CV_ENCOUTER_TYPE.CODE_VALUE
	)
	AND (
		ENCOUNTER.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCNTR_ALIAS.END_EFFECTIVE_DT_TM > SYSDATE
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619
	)
	AND (
		ORDERS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ORDERS.ORIG_ORDER_DT_TM >= ENCNTR_LOC_HIST.TRANSACTION_DT_TM
		AND (ORDERS.ORIG_ORDER_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM)
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				ELH.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
				AND ORDERS.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.ACTIVE_IND = 1
		)
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = CV_NURSE_UNIT.CODE_VALUE
	)
	AND (
		ORDERS.ORDER_ID = ORDER_ACTION.ORDER_ID
		AND ORDER_ACTION.ACTION_TYPE_CD = 1376
		AND ORDER_ACTION.ORDER_PROVIDER_ID = PRSNL.PERSON_ID
	)
	AND PRSNL.POSITION_CD = CV_POSITION.CODE_VALUE
	AND (
		ORDERS.ORDER_ID = OD_DISPENSEQTY.ORDER_ID(+)
		AND OD_DISPENSEQTY.ACTION_SEQUENCE(+) = 1
		AND OD_DISPENSEQTY.OE_FIELD_MEANING_ID(+) = 2015
	)
	AND (
		ORDERS.ORDER_ID = OD_DISPENSEQTYUNIT.ORDER_ID(+)
		AND OD_DISPENSEQTYUNIT.ACTION_SEQUENCE(+) = 1
		AND OD_DISPENSEQTYUNIT.OE_FIELD_MEANING_ID(+) = 2102
	)
	AND (
		ORDERS.ORDER_ID = OD_DURATION.ORDER_ID(+)
		AND OD_DURATION.ACTION_SEQUENCE(+) = 1
		AND OD_DURATION.OE_FIELD_MEANING_ID(+) = 2061
	)
	AND (
		ORDERS.ORDER_ID = OD_DURATIONUNIT.ORDER_ID(+)
		AND OD_DURATIONUNIT.ACTION_SEQUENCE(+) = 1
		AND OD_DURATIONUNIT.OE_FIELD_MEANING_ID(+) = 2062
	)
	AND (
		ORDERS.ORDER_ID = OD_FREQ.ORDER_ID(+)
		AND OD_FREQ.ACTION_SEQUENCE(+) = 1
		AND OD_FREQ.OE_FIELD_MEANING_ID(+) = 2011
	)
	AND (
		ORDERS.ORDER_ID = OD_NBRREFILLS.ORDER_ID(+)
		AND OD_NBRREFILLS.ACTION_SEQUENCE(+) = 1
		AND OD_NBRREFILLS.OE_FIELD_MEANING_ID(+) = 67
	)
	AND (
		ORDERS.ORDER_ID = OD_PRNINSTRUCTIONS.ORDER_ID(+)
		AND OD_PRNINSTRUCTIONS.ACTION_SEQUENCE(+) = 1
		AND OD_PRNINSTRUCTIONS.OE_FIELD_MEANING_ID(+) = 2101
	)
	AND (
		ORDERS.ORDER_ID = OD_STRENGTHDOSE.ORDER_ID(+)
		AND OD_STRENGTHDOSE.ACTION_SEQUENCE(+) = 1
		AND OD_STRENGTHDOSE.OE_FIELD_MEANING_ID(+) = 2056
	)
	AND (
		ORDERS.ORDER_ID = OD_STRENGTHDOSEUNIT.ORDER_ID(+)
		AND OD_STRENGTHDOSEUNIT.ACTION_SEQUENCE(+) = 1
		AND OD_STRENGTHDOSEUNIT.OE_FIELD_MEANING_ID(+) = 2057
	)
	AND (
		ORDERS.ORDER_ID = OD_VOLUMEDOSE.ORDER_ID(+)
		AND OD_VOLUMEDOSE.ACTION_SEQUENCE(+) = 1
		AND OD_VOLUMEDOSE.OE_FIELD_MEANING_ID(+) = 2058
	)
	AND (
		ORDERS.ORDER_ID = OD_VOLUMEDOSEUNIT.ORDER_ID(+)
		AND OD_VOLUMEDOSEUNIT.ACTION_SEQUENCE(+) = 1
		AND OD_VOLUMEDOSEUNIT.OE_FIELD_MEANING_ID(+) = 2059
	)
	AND	(
		ORDERS.ORIG_ORDER_DT_TM + 0
			BETWEEN DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', pi_to_gmt(TRUNC(SYSDATE), pi_time_zone(2, @Variable('BOUSER'))),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - 1, pi_time_zone(2, @Variable('BOUSER'))),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE-1, 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
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
		AND ORDERS.ORIG_ORDER_DT_TM
			BETWEEN DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', TRUNC(SYSDATE),
				'Yesterday', TRUNC(SYSDATE) - 1,
				'Week to Date', TRUNC(SYSDATE, 'DAY'),
				'Last Week', TRUNC(SYSDATE - 7, 'DAY'),
				'Last Month', TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'),
				'Month to Date', TRUNC(SYSDATE-1, 'MONTH'),
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
