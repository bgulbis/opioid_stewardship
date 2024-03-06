WITH LA_DOSES AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.PERSON_ID,
		CLINICAL_EVENT.EVENT_ID,
		CLINICAL_EVENT.EVENT_END_DT_TM,
		CLINICAL_EVENT.EVENT_CD,
		pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
		ORDERS.CATALOG_CD,
		LOWER(pi_get_cv_display(ORDERS.CATALOG_CD)) AS ORDER_MED,
		ORDERS.ORDERED_AS_MNEMONIC,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_FACILITY_CD) AS FACILITY,
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
		CE_MED_RESULT.ADMIN_ROUTE_CD,
		pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ROUTE,
		ORDER_PRODUCT.ITEM_ID,
		MED_IDENTIFIER.VALUE AS MED_PRODUCT,
		CASE
			WHEN ORDERS.TEMPLATE_ORDER_ID = 0 THEN CLINICAL_EVENT.ORDER_ID
			ELSE ORDERS.TEMPLATE_ORDER_ID
		END AS ORIG_ORDER_ID
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST,
		ENCOUNTER,
		MED_IDENTIFIER,
		ORDER_PRODUCT,
		ORDERS
	WHERE
		CLINICAL_EVENT.EVENT_CD IN (
			37556352, -- buprenorphine
			37556956, -- FENTanyl
			37557538, -- methadone
			37557620, -- morphine Sulfate
			37557746, -- OXYcodone
			61253250, -- MORPhine
			117038568, -- buprenorphine-naloxone
			117038771 -- HYDROcodone
		)
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			DECODE(
				@Prompt('Choose date range', 'A', {'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date', 'D', , mono, free, persistent, {'12/01/2020 00:00:00'}, User:1),
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
						@Prompt('Enter end date', 'D', , mono, free, persistent, {'01/01/2021 00:00:00'}, User:2),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					) - 1/86400,
					pi_time_zone(1, 'America/Chicago')
				)
			)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND (CE_MED_RESULT.ADMIN_DOSAGE > 0 OR CE_MED_RESULT.IV_EVENT_CD > 0)
		AND CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND ENCOUNTER.ENCNTR_TYPE_CLASS_CD = 42631 -- Inpatient
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
			-- 3310, -- HH HERMANN
			3796 -- HC Childrens
			-- 3821, -- HH Clinics
			-- 3822, -- HH Trans Care
			-- 3823 -- HH Rehab
		)
		AND CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
		-- AND CLINICAL_EVENT.ORDER_ID = ORDER_PRODUCT.ORDER_ID
        AND CASE
			    WHEN ORDERS.TEMPLATE_ORDER_ID = 0 THEN ORDERS.ORDER_ID
			    ELSE ORDERS.TEMPLATE_ORDER_ID
		    END = ORDER_PRODUCT.ORDER_ID
		AND ORDER_PRODUCT.ITEM_ID IN (
			70713575, -- buprenorphine 8mg tab
			182750880, -- buprenorphine 2 mg TAB
			80271818, -- buprenorphine-naloxone (2mg-0.5mg) TAB
			89644187, -- buprenorphine-naloxone 8 mg - 2 mg Tab
			186603017, -- buprenorphine-naloxone (8mg-2mg) FILM ORAL
			2938344, -- fentaNYL 100 microgram/hr PATCH
			2931623, -- fentaNYL 75 microgram/hr PATCH
			2939684, -- fentaNYL 25 microgram/hr PATCH
			2939721, -- fentaNYL 50 microgram/hr PATCH
			46394496, -- fentaNYL 12 microgram/hr PATCH
			2937609, -- methadone 10 mg TAB
			2937648, -- methadone 5 mg TAB
			2970105, -- methadone 5 mg/5 ml oral SOLN
			23879032, -- methadone *1mg/1ml oral syringes
			2818166, -- MORPhine sulfate 15 mg CRT
			2818209, -- MORPhine sulfate 30 mg CRT
			2938272, -- oxyCODONE 20 mg ERT
			2960111, -- oxyCODONE 10 mg ERT
			98741187 -- oxyCODONE 15 mg ERT
		)
		AND ORDER_PRODUCT.ITEM_ID = MED_IDENTIFIER.ITEM_ID
        AND ORDER_PRODUCT.MED_PRODUCT_ID = MED_IDENTIFIER.MED_PRODUCT_ID
		AND MED_IDENTIFIER.MED_IDENTIFIER_TYPE_CD = 1564 -- Description
		-- AND MED_IDENTIFIER.MED_PRODUCT_ID = 0
), ALL_DOSES AS (
	SELECT DISTINCT
		LA_DOSES.ENCNTR_ID,
		ORDER_PRODUCT.ITEM_ID,
		pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS DOSE_DATETIME,
		CLINICAL_EVENT.EVENT_ID
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		LA_DOSES,
		ORDER_PRODUCT,
		ORDERS
	WHERE
		LA_DOSES.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
		AND LA_DOSES.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			37556352, -- buprenorphine
			37556956, -- FENTanyl
			37557538, -- methadone
			37557620, -- morphine Sulfate
			37557746, -- OXYcodone
			61253250, -- MORPhine
			117038568, -- buprenorphine-naloxone
			117038771 -- HYDROcodone
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND (CE_MED_RESULT.ADMIN_DOSAGE > 0 OR CE_MED_RESULT.IV_EVENT_CD > 0)
		AND CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
		-- AND CLINICAL_EVENT.ORDER_ID = ORDER_PRODUCT.ORDER_ID
		AND CASE
				WHEN ORDERS.TEMPLATE_ORDER_ID = 0 THEN ORDERS.ORDER_ID
				ELSE ORDERS.TEMPLATE_ORDER_ID
			END = ORDER_PRODUCT.ORDER_ID
		AND ORDER_PRODUCT.ITEM_ID IN (
			70713575, -- buprenorphine 8mg tab
			182750880, -- buprenorphine 2 mg TAB
			80271818, -- buprenorphine-naloxone (2mg-0.5mg) TAB
			89644187, -- buprenorphine-naloxone 8 mg - 2 mg Tab
			186603017, -- buprenorphine-naloxone (8mg-2mg) FILM ORAL
			2938344, -- fentaNYL 100 microgram/hr PATCH
			2931623, -- fentaNYL 75 microgram/hr PATCH
			2939684, -- fentaNYL 25 microgram/hr PATCH
			2939721, -- fentaNYL 50 microgram/hr PATCH
			46394496, -- fentaNYL 12 microgram/hr PATCH
			2937609, -- methadone 10 mg TAB
			2937648, -- methadone 5 mg TAB
			2970105, -- methadone 5 mg/5 ml oral SOLN
			23879032, -- methadone *1mg/1ml oral syringes
			2818166, -- MORPhine sulfate 15 mg CRT
			2818209, -- MORPhine sulfate 30 mg CRT
			2938272, -- oxyCODONE 20 mg ERT
			2960111, -- oxyCODONE 10 mg ERT
			98741187 -- oxyCODONE 15 mg ERT
		)
), FIRST_DOSES AS (
	SELECT
		ENCNTR_ID,
		ITEM_ID,
		MIN(DOSE_DATETIME) AS FIRST_DOSE_DATETIME
	FROM
		ALL_DOSES
	GROUP BY
		ENCNTR_ID,
		ITEM_ID
), HOME_MEDS AS (
	SELECT DISTINCT
		LA_DOSES.ENCNTR_ID,
		ORDERS.ORDER_ID,
		ORDERS.CATALOG_CD,
		pi_get_cv_display(ORDERS.CATALOG_CD) AS MEDICATION,
		ORDERS.ORDERED_AS_MNEMONIC,
		ORDERS.PRN_IND,
		FREQUENCY_SCHEDULE.FREQUENCY_CD
	FROM
		FREQUENCY_SCHEDULE,
		LA_DOSES,
		ORDERS,
		PI_THERA_CLASS_VIEW
	WHERE
		LA_DOSES.PERSON_ID = ORDERS.PERSON_ID
		AND ORDERS.CATALOG_TYPE_CD = 1363 -- Pharmacy
		AND LA_DOSES.ENCNTR_ID = ORDERS.ENCNTR_ID
		AND ORDERS.ORIG_ORD_AS_FLAG = 2 -- Recorded / Home Meds
		AND ORDERS.CATALOG_CD = PI_THERA_CLASS_VIEW.DRUG_CAT_CD  
		AND PI_THERA_CLASS_VIEW.DRUG_CAT IN ('narcotic analgesic combinations', 'narcotic analgesics')		
		AND ORDERS.FREQUENCY_ID = FREQUENCY_SCHEDULE.FREQUENCY_ID(+)
), ORD_DETAILS AS (
	SELECT DISTINCT
		ORDER_DETAIL.ORDER_ID,
		ORDER_DETAIL.OE_FIELD_MEANING,
		ORDER_DETAIL.OE_FIELD_DISPLAY_VALUE
	FROM
		HOME_MEDS,
		ORDER_DETAIL
	WHERE
		HOME_MEDS.ORDER_ID = ORDER_DETAIL.ORDER_ID
		AND ORDER_DETAIL.ACTION_SEQUENCE = 1
), ORD_DETAIL_PIVOT AS (
	SELECT * FROM ORD_DETAILS
	PIVOT(
		MIN(OE_FIELD_DISPLAY_VALUE) FOR OE_FIELD_MEANING IN (
			'DISPENSEQTY' AS DISPENSE_QTY,
			'DISPENSEQTYUNIT' AS DISPENSE_UNIT,
			'DURATION' AS DURATION,
			'DURATIONUNIT' AS DURATION_UNIT,
			'FREETXTDOSE' AS FREETXT_DOSE,
			'PRNINSTRUCTIONS' AS PRN_INSTR,
			'NBRREFILLS' AS REFILLS,
			'RXROUTE' AS ROUTE,
			'SPECINX' AS SPECIAL_INSTR,
			'STRENGTHDOSE' AS DOSE,
			'STRENGTHDOSEUNIT' AS DOSE_UNIT,
			'VOLUMEDOSE' AS VOLUME,
			'VOLUMEDOSEUNIT' AS VOLUME_UNIT
		)
	)
), HOME_MED_ORDERS AS (
	SELECT DISTINCT
		HOME_MEDS.ENCNTR_ID,
		HOME_MEDS.ORDER_ID,
		-- pi_from_gmt(LAST_RX.ORIG_ORDER_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS ORDER_DATETIME,
		-- TRUNC(pi_from_gmt(LAST_RX.ORIG_ORDER_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'MONTH') AS ORDER_MONTH,
		LOWER(pi_get_cv_display(HOME_MEDS.CATALOG_CD)) AS MEDICATION,
		LOWER(HOME_MEDS.ORDERED_AS_MNEMONIC) AS ORDERED_AS,
		-- TO_NUMBER(ORD_DETAIL_PIVOT.DOSE) AS DOSE,
		ORD_DETAIL_PIVOT.DOSE AS DOSE,
		LOWER(ORD_DETAIL_PIVOT.DOSE_UNIT) AS DOSE_UNIT,
		-- TO_NUMBER(ORD_DETAIL_PIVOT.VOLUME) AS VOLUME,
		ORD_DETAIL_PIVOT.VOLUME AS VOLUME,
		LOWER(ORD_DETAIL_PIVOT.VOLUME_UNIT) AS VOLUME_UNIT,
		LOWER(ORD_DETAIL_PIVOT.FREETXT_DOSE) AS FREETXT_DOSE,
		REGEXP_REPLACE(LOWER(ORD_DETAIL_PIVOT.FREETXT_DOSE), '[[:space:]]', '') AS FREETXT_DOSE_NOSPACE,
		LOWER(ORD_DETAIL_PIVOT.ROUTE) AS ROUTE,
		LOWER(pi_get_cv_display(HOME_MEDS.FREQUENCY_CD)) AS FREQ,
		CASE HOME_MEDS.PRN_IND
			WHEN 1 THEN 0.5
			ELSE 1
		END AS PRN,
		LOWER(ORD_DETAIL_PIVOT.PRN_INSTR) AS PRN_INSTR,
		LOWER(ORD_DETAIL_PIVOT.SPECIAL_INSTR) AS SPECIAL_INSTR,
		REGEXP_REPLACE(LOWER(ORD_DETAIL_PIVOT.SPECIAL_INSTR), '[[:space:]]', '') AS SPECIAL_INSTR_NOSPACE
	FROM
		HOME_MEDS,
		ORD_DETAIL_PIVOT
	WHERE
		HOME_MEDS.ORDER_ID = ORD_DETAIL_PIVOT.ORDER_ID(+)
), HOME_MED_ORDERS_MOD AS (
	SELECT
		HOME_MED_ORDERS.*,
		CASE 
			WHEN REGEXP_INSTR(ORDERED_AS, '30 mg/5 ml') > 0 THEN 30 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '20 mg/5 ml') > 0 THEN 20 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '12 mg/5 ml') > 0 THEN 12 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '5 mg/5 ml') > 0 THEN 5 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '2.5 mg/5 ml') > 0 THEN 2.5 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '10 mg/15 ml') > 0 THEN 10 / 15
			WHEN REGEXP_INSTR(ORDERED_AS, '7.5 mg/15 ml|7.5 mg-325 mg/15 ml') > 0 THEN 7.5 / 15
			WHEN REGEXP_INSTR(ORDERED_AS, '100( )?mg') > 0 THEN 100
			WHEN REGEXP_INSTR(ORDERED_AS, '60( )?mg|#4') > 0 THEN 60
			WHEN REGEXP_INSTR(ORDERED_AS, '50( )?mg') > 0 THEN 37.5
			WHEN REGEXP_INSTR(ORDERED_AS, '37.5( )?mg') > 0 THEN 50
			WHEN REGEXP_INSTR(ORDERED_AS, '30( )?mg|#3') > 0 THEN 30
			WHEN REGEXP_INSTR(ORDERED_AS, '27( )?mg') > 0 THEN 27
			WHEN REGEXP_INSTR(ORDERED_AS, '20( )?mg') > 0 THEN 20
			WHEN REGEXP_INSTR(ORDERED_AS, '18( )?mg') > 0 THEN 18
			WHEN REGEXP_INSTR(ORDERED_AS, '15( )?mg') > 0 THEN 15
			WHEN REGEXP_INSTR(ORDERED_AS, '12( )?mg') > 0 THEN 12
			WHEN REGEXP_INSTR(ORDERED_AS, '10( )?mg|10/325') > 0 THEN 10
			WHEN REGEXP_INSTR(ORDERED_AS, '8( )?mg') > 0 THEN 8
			WHEN REGEXP_INSTR(ORDERED_AS, '7.5( )?mg|7.5/325') > 0 THEN 7.5
			WHEN REGEXP_INSTR(ORDERED_AS, '5( )?mg|5/325') > 0 THEN 5
			WHEN REGEXP_INSTR(ORDERED_AS, '4( )?mg') > 0 THEN 4
			WHEN REGEXP_INSTR(ORDERED_AS, '2.5( )?mg') > 0 THEN 2.5
			WHEN REGEXP_INSTR(ORDERED_AS, '2( )?mg') > 0 THEN 2
			WHEN REGEXP_INSTR(ORDERED_AS, '1( )?mg') > 0 THEN 1
			WHEN REGEXP_INSTR(ORDERED_AS, '200( )?mg') > 0 THEN 200
			WHEN REGEXP_INSTR(ORDERED_AS, '75 mcg/hr') > 0 THEN 75
			WHEN REGEXP_INSTR(ORDERED_AS, '50 mcg/hr') > 0 THEN 50
			WHEN REGEXP_INSTR(ORDERED_AS, '25 mcg/hr') > 0 THEN 25
			WHEN REGEXP_INSTR(ORDERED_AS, '20 mcg/hr') > 0 THEN 20
			WHEN DOSE IS NOT NULL THEN TO_NUMBER(DOSE)
		END AS STRENGTH,
		CASE
			WHEN VOLUME IS NOT NULL THEN TO_NUMBER(VOLUME)
			WHEN REGEXP_INSTR(FREETXT_DOSE_NOSPACE, '2tab|2cap') > 0 THEN 2 
			WHEN REGEXP_INSTR(FREETXT_DOSE_NOSPACE, '1.5tab|1.5cap') > 0 THEN 1.5
			WHEN REGEXP_INSTR(FREETXT_DOSE_NOSPACE, '1tab|1cap') > 0 THEN 1
			WHEN REGEXP_INSTR(FREETXT_DOSE_NOSPACE, '0.5tab|0.5cap') > 0 THEN 0.5
			WHEN REGEXP_INSTR(FREETXT_DOSE, '1.5') > 0 THEN 1.5
			WHEN REGEXP_INSTR(FREETXT_DOSE, '0.5') > 0 THEN 0.5
			WHEN REGEXP_INSTR(FREETXT_DOSE, 'see instructions') > 0 THEN 
				CASE
					WHEN REGEXP_INSTR(SPECIAL_INSTR_NOSPACE, '2tab|2cap') > 0 THEN 2 
					WHEN REGEXP_INSTR(SPECIAL_INSTR_NOSPACE, '1.5tab|1.5cap') > 0 THEN 1.5 
					WHEN REGEXP_INSTR(SPECIAL_INSTR_NOSPACE, '1tab|1cap') > 0 THEN 1 
					WHEN REGEXP_INSTR(SPECIAL_INSTR_NOSPACE, '0.5tab|0.5cap') > 0 THEN 0.5
				END
			ELSE 1
		END AS AMOUNT,
		CASE
			WHEN REGEXP_INSTR(MEDICATION, 'codeine') > 0 THEN 0.15
			WHEN REGEXP_INSTR(MEDICATION, 'hydromorphone') > 0 THEN 4
			WHEN REGEXP_INSTR(MEDICATION, 'morphine') > 0 THEN 1
			WHEN REGEXP_INSTR(MEDICATION, 'tramadol') > 0 THEN 0.1
			WHEN REGEXP_INSTR(MEDICATION, 'hydrocodone') > 0 THEN 1
			WHEN REGEXP_INSTR(MEDICATION, 'oxycodone') > 0 THEN 1.5
			WHEN REGEXP_INSTR(MEDICATION, 'buprenorphine-naloxone') > 0 THEN 30
			WHEN REGEXP_INSTR(MEDICATION, 'fentanyl') > 0 THEN 
				CASE
					WHEN ROUTE = 'top' OR ROUTE = 'transdermal' THEN 7.2
				END
			WHEN REGEXP_INSTR(MEDICATION, 'methadone') > 0 THEN
				CASE
					WHEN DOSE <= 20 THEN 4
					WHEN DOSE <= 40 THEN 8
					WHEN DOSE <= 60 THEN 10
					WHEN DOSE > 60 THEN 12
					ELSE 3
				END
			WHEN REGEXP_INSTR(MEDICATION, 'buprenorphine') > 0 THEN
				CASE
					WHEN ROUTE = 'top' OR ROUTE = 'transdermal' THEN 12.6
				END
		END AS MME_CONVERSION,
		CASE
			WHEN REGEXP_INSTR(FREQ, 'q4') > 0
				OR REGEXP_INSTR(SPECIAL_INSTR_NOSPACE, 'q4') > 0 THEN 6
			WHEN REGEXP_INSTR(FREQ, 'q6') > 0
				OR REGEXP_INSTR(FREQ, 'qid') > 0
				OR REGEXP_INSTR(SPECIAL_INSTR_NOSPACE, 'q6') > 0 THEN 4
			WHEN REGEXP_INSTR(FREQ, 'q8') > 0
				OR REGEXP_INSTR(FREQ, 'tid') > 0
				OR REGEXP_INSTR(SPECIAL_INSTR_NOSPACE, 'q8') > 0 THEN 3
			WHEN REGEXP_INSTR(FREQ, 'q12') > 0
				OR REGEXP_INSTR(FREQ, 'bid') > 0
				OR REGEXP_INSTR(SPECIAL_INSTR_NOSPACE, 'q12') > 0 THEN 2
			WHEN REGEXP_INSTR(FREQ, 'q24') > 0
				OR REGEXP_INSTR(FREQ, 'daily') > 0
				OR REGEXP_INSTR(FREQ, 'bedtime') > 0
				OR REGEXP_INSTR(FREQ, 'once') > 0
				OR REGEXP_INSTR(SPECIAL_INSTR_NOSPACE, 'q24') > 0 THEN 1
		END AS ADMIN_PER_DAY
	FROM
		HOME_MED_ORDERS
), HOME_MEDS_MME AS (
	SELECT
		HOME_MED_ORDERS_MOD.*,
		CASE 
			WHEN DOSE_UNIT = 'mg' THEN DOSE * MME_CONVERSION * (ADMIN_PER_DAY * PRN)
			ELSE STRENGTH * AMOUNT * MME_CONVERSION * (ADMIN_PER_DAY * PRN)
		END AS HOME_MMED
	FROM HOME_MED_ORDERS_MOD
), INPT_OPIOIDS AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.EVENT_ID,
		pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS EVENT_END_DT_TM,
		TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) AS EVENT_END_DT,
		CLINICAL_EVENT.EVENT_CD,
		LOWER(pi_get_cv_display(CLINICAL_EVENT.EVENT_CD)) AS MEDICATION,
		LOWER(ORDERS.ORDERED_AS_MNEMONIC) AS ORDERED_AS,
		-- MED_IDENTIFIER.VALUE AS MED_PRODUCT,
		CE_MED_RESULT.ADMIN_DOSAGE,
		CE_MED_RESULT.DOSAGE_UNIT_CD,
		pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSAGE_UNIT,
		CE_MED_RESULT.INFUSION_RATE,
		CE_MED_RESULT.INFUSION_UNIT_CD,
		pi_get_cv_display(CE_MED_RESULT.INFUSION_UNIT_CD) AS INFUSION_UNIT,
		CE_MED_RESULT.IV_EVENT_CD,
		pi_get_cv_display(CE_MED_RESULT.IV_EVENT_CD) AS IV_EVENT,
		CE_MED_RESULT.ADMIN_ROUTE_CD,
		CASE 
			WHEN pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) IN ('IV', 'IVP', 'IVPB') THEN 'iv'
			ELSE LOWER(pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD))
		END AS ROUTE
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		LA_DOSES,
		-- MED_IDENTIFIER,
		-- ORDER_PRODUCT,
		ORDERS
	WHERE
		LA_DOSES.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
		AND LA_DOSES.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			37556014, -- acetaminophen-codeine
			37556016, -- acetaminophen-hydrocodone
			37556017, -- acetaminophen-oxycodone
			37556018, -- acetaminophen-pentazocine
			37556023, -- acetaminophen-tramadol
			37556061, -- alfentanil
			37556193, -- aspirin-codeine
			37556197, -- aspirin-oxycodone
			37556198, -- aspirin-pentazocine
			37556352, -- buprenorphine
			37556359, -- butorphanol
			37556595, -- codeine
			37556814, -- droperidol-fentanyl
			37556956, -- FENTanyl
			37557191, -- HYDROcodone-ibuprofen
			37557204, -- HYDROmorphone
			37557424, -- levorphanol
			37557517, -- meperidine
			37557519, -- meperidine-promethazine
			37557538, -- methadone
			37557620, -- morphine Sulfate
			37557645, -- nalbuphine
			37557648, -- naloxone-pentazocine
			37557727, -- opium
			37557746, -- OXYcodone
			37557790, -- pentazocine
			37557973, -- remifentanil
			37558121, -- sufentanil
			37558239, -- tramadol
			99783187, -- morphine liposomal
			103856893, -- fentanyl-ropivacaine
			117038566, -- bupivacaine-fentanyl
			117038567, -- bupivacaine-hydromorphone
			117038568, -- buprenorphine-naloxone
			117038681, -- dihydrocodeine
			117038771, -- HYDROcodone
			117038789, -- ibuprofen-oxycodone
			117038901, -- oxymorphone
			405732895, -- tapentadol
			423546842, -- morphine-naltrexone
			538590483, -- ropivacaine-sufentanil
			1651062812, -- HYDROmorphone-ropivacaine
			2180033613, -- bupivacaine-SUFentanil
			3135779565 -- acetaminophen-benzhydrocodone
		)
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			TRUNC(LA_DOSES.EVENT_END_DT_TM) - 3
			AND TRUNC(LA_DOSES.EVENT_END_DT_TM)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
		-- AND CLINICAL_EVENT.ORDER_ID = ORDER_PRODUCT.ORDER_ID(+)
		-- AND ORDER_PRODUCT.ITEM_ID = MED_IDENTIFIER.ITEM_ID(+)
		-- AND MED_IDENTIFIER.MED_IDENTIFIER_TYPE_CD(+) = 1564 -- Description
		-- AND MED_IDENTIFIER.MED_PRODUCT_ID(+) = 0
/* 		AND (
			CE_MED_RESULT.ADMIN_DOSAGE > 0 
			OR CE_MED_RESULT.IV_EVENT_CD IN (
				688706, -- Begin Bag
				688709 -- Rate Change
			)
		) */
), INTERMIT_DOSES AS (
	SELECT
		ENCNTR_ID,
		EVENT_END_DT,
		MEDICATION,
		ORDERED_AS,
		-- MED_PRODUCT,
		ROUTE,
		DOSAGE_UNIT_CD,
		SUM(ADMIN_DOSAGE) AS TOTAL_DOSE,
		CASE 
			WHEN REGEXP_INSTR(ORDERED_AS, '30 mg/5 ml') > 0 THEN 30 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '20 mg/5 ml') > 0 THEN 20 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '12 mg/5 ml') > 0 THEN 12 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '5 mg/5 ml') > 0 THEN 5 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '2.5 mg/5 ml') > 0 THEN 2.5 / 5
			WHEN REGEXP_INSTR(ORDERED_AS, '10 mg/15 ml') > 0 THEN 10 / 15
			WHEN REGEXP_INSTR(ORDERED_AS, '7.5 mg/15 ml|7.5 mg-325 mg/15 ml') > 0 THEN 7.5 / 15
			WHEN REGEXP_INSTR(ORDERED_AS, '100( )?mg') > 0 THEN 100
			WHEN REGEXP_INSTR(ORDERED_AS, '60( )?mg|#4') > 0 THEN 60
			WHEN REGEXP_INSTR(ORDERED_AS, '50( )?mg') > 0 THEN 37.5
			WHEN REGEXP_INSTR(ORDERED_AS, '37.5( )?mg') > 0 THEN 50
			WHEN REGEXP_INSTR(ORDERED_AS, '30( )?mg|#3') > 0 THEN 30
			WHEN REGEXP_INSTR(ORDERED_AS, '27( )?mg') > 0 THEN 27
			WHEN REGEXP_INSTR(ORDERED_AS, '20( )?mg') > 0 THEN 20
			WHEN REGEXP_INSTR(ORDERED_AS, '18( )?mg') > 0 THEN 18
			WHEN REGEXP_INSTR(ORDERED_AS, '15( )?mg') > 0 THEN 15
			WHEN REGEXP_INSTR(ORDERED_AS, '12( )?mg') > 0 THEN 12
			WHEN REGEXP_INSTR(ORDERED_AS, '10( )?mg|10/325') > 0 THEN 10
			WHEN REGEXP_INSTR(ORDERED_AS, '8( )?mg') > 0 THEN 8
			WHEN REGEXP_INSTR(ORDERED_AS, '7.5( )?mg|7.5/325') > 0 THEN 7.5
			WHEN REGEXP_INSTR(ORDERED_AS, '5( )?mg|5/325') > 0 THEN 5
			WHEN REGEXP_INSTR(ORDERED_AS, '4( )?mg') > 0 THEN 4
			WHEN REGEXP_INSTR(ORDERED_AS, '2.5( )?mg') > 0 THEN 2.5
			WHEN REGEXP_INSTR(ORDERED_AS, '2( )?mg') > 0 THEN 2
			WHEN REGEXP_INSTR(ORDERED_AS, '1( )?mg') > 0 THEN 1
			WHEN REGEXP_INSTR(ORDERED_AS, '200( )?mg') > 0 THEN 200
			WHEN REGEXP_INSTR(ORDERED_AS, '75 mcg/hr') > 0 THEN 75
			WHEN REGEXP_INSTR(ORDERED_AS, '50 mcg/hr') > 0 THEN 50
			WHEN REGEXP_INSTR(ORDERED_AS, '25 mcg/hr') > 0 THEN 25
			WHEN REGEXP_INSTR(ORDERED_AS, '20 mcg/hr') > 0 THEN 20
		END AS STRENGTH
	FROM
		INPT_OPIOIDS
	WHERE
		IV_EVENT_CD = 0
	GROUP BY
		ENCNTR_ID,
		EVENT_END_DT,
		MEDICATION,
		ORDERED_AS,
		-- MED_PRODUCT,
		ROUTE,
		DOSAGE_UNIT_CD
), INTERMIT_DAILY_DOSES AS (
	SELECT
		ENCNTR_ID,
		EVENT_END_DT,
		MEDICATION,
		ORDERED_AS,
		STRENGTH,
		ROUTE,
		pi_get_cv_display(DOSAGE_UNIT_CD) AS DOSE_UNIT,
		TOTAL_DOSE,
		CASE
			WHEN pi_get_cv_display(DOSAGE_UNIT_CD) IN ('tab', 'cap') THEN TOTAL_DOSE * STRENGTH
			ELSE TOTAL_DOSE
		END AS DAILY_DOSE
	FROM INTERMIT_DOSES
), INTERMIT_MME AS (
	SELECT
		INTERMIT_DAILY_DOSES.*,
		CASE
			WHEN ROUTE = 'iv' THEN
				CASE
					WHEN REGEXP_INSTR(MEDICATION, 'hydromorphone') > 0 THEN 20
					WHEN REGEXP_INSTR(MEDICATION, 'morphine') > 0 THEN 3
					WHEN REGEXP_INSTR(MEDICATION, 'fentanyl') > 0 THEN 0.3
					WHEN REGEXP_INSTR(MEDICATION, 'remifentanil') > 0 THEN 300
				END
			WHEN ROUTE = 'top' THEN
				CASE
					WHEN REGEXP_INSTR(MEDICATION, 'buprenorphine') > 0 THEN 12.6
					WHEN REGEXP_INSTR(MEDICATION, 'fentanyl') > 0 THEN 7.2
				END			
			ELSE
				CASE
					WHEN REGEXP_INSTR(MEDICATION, 'codeine') > 0 THEN 0.15
					WHEN REGEXP_INSTR(MEDICATION, 'hydromorphone') > 0 THEN 4
					WHEN REGEXP_INSTR(MEDICATION, 'morphine') > 0 THEN 1
					WHEN REGEXP_INSTR(MEDICATION, 'tramadol') > 0 THEN 0.1
					WHEN REGEXP_INSTR(MEDICATION, 'hydrocodone') > 0 THEN 1
					WHEN REGEXP_INSTR(MEDICATION, 'oxycodone') > 0 THEN 1.5
					WHEN REGEXP_INSTR(MEDICATION, 'buprenorphine-naloxone') > 0 THEN 30
					WHEN REGEXP_INSTR(MEDICATION, 'methadone') > 0 THEN
						CASE
							WHEN DAILY_DOSE <= 20 THEN 4
							WHEN DAILY_DOSE <= 40 THEN 8
							WHEN DAILY_DOSE <= 60 THEN 10
							WHEN DAILY_DOSE > 60 THEN 12
							ELSE 3
						END
				END
		END AS MME_CONVERSION
	FROM INTERMIT_DAILY_DOSES
), INTERMIT_MME_TOTAL AS (
	SELECT
		INTERMIT_MME.*,
		DAILY_DOSE * MME_CONVERSION AS MME_DAY
	FROM INTERMIT_MME
), INTERMIT_MME_DAY AS (
	SELECT
		ENCNTR_ID,
		EVENT_END_DT,
		SUM(MME_DAY) AS MME_DAY
	FROM INTERMIT_MME_TOTAL
	GROUP BY
		ENCNTR_ID,
		EVENT_END_DT
), INTERMIT_MME_AVG AS (
	SELECT
		ENCNTR_ID,
		AVG(MME_DAY) AS MMED_AVG
	FROM INTERMIT_MME_DAY
	GROUP BY ENCNTR_ID
), ON_AT_HOME AS (
	SELECT
		LA_DOSES.ENCNTR_ID,
		LA_DOSES.ORDER_MED AS INPT_MED,
		HOME_MED_ORDERS.MEDICATION AS HM_MED,
		CASE
			WHEN LA_DOSES.ORDER_MED = HOME_MED_ORDERS.MEDICATION THEN 1
			ELSE 0
		END AS ON_AT_HOME
	FROM
		HOME_MED_ORDERS,
		LA_DOSES
	WHERE
		LA_DOSES.ENCNTR_ID = HOME_MED_ORDERS.ENCNTR_ID
), ON_AT_HOME_SUM AS (
	SELECT
		ENCNTR_ID,
		INPT_MED,
		CASE
			WHEN SUM(ON_AT_HOME) > 0 THEN 1
			ELSE 0
		END AS ON_AT_HOME
	FROM ON_AT_HOME
	GROUP BY
		ENCNTR_ID,
		INPT_MED
), HOME_MEDS_MME_SUM AS (
	SELECT
		ENCNTR_ID,
		MEDICATION,
		MAX(HOME_MMED) AS HOME_MMED
	FROM HOME_MEDS_MME
	GROUP BY
		ENCNTR_ID,
		MEDICATION
), HOME_MEDS_MME_TOTAL AS (
	SELECT
		ENCNTR_ID,
		SUM(HOME_MMED) AS HOME_MMED
	FROM HOME_MEDS_MME_SUM
	GROUP BY
		ENCNTR_ID
)

SELECT
	LA_DOSES.FACILITY,
	ENCNTR_ALIAS.ALIAS AS FIN,
	-- LA_DOSES.MEDICATION,
	LA_DOSES.MED_PRODUCT,
	TRUNC(FIRST_DOSES.FIRST_DOSE_DATETIME) AS FIRST_DATETIME,
	CASE
		WHEN ON_AT_HOME_SUM.ON_AT_HOME = 1 THEN 'Yes'
		WHEN ON_AT_HOME_SUM.ON_AT_HOME = 0 THEN 'No'
	END AS ON_AT_HOME,
	HOME_MEDS_MME_TOTAL.HOME_MMED,
	INTERMIT_MME_AVG.MMED_AVG,
	CASE
		WHEN ON_AT_HOME_SUM.ON_AT_HOME = 1 OR HOME_MEDS_MME_TOTAL.HOME_MMED >= 60 OR INTERMIT_MME_AVG.MMED_AVG >= 60 THEN 'Yes'
		ELSE 'No'
	END AS APPROPRIATE
FROM
	ENCNTR_ALIAS,
	FIRST_DOSES,
	HOME_MEDS_MME_TOTAL,
	INTERMIT_MME_AVG,
	LA_DOSES,
	ON_AT_HOME_SUM
WHERE
	LA_DOSES.ENCNTR_ID = ON_AT_HOME_SUM.ENCNTR_ID(+)
	AND LA_DOSES.ORDER_MED = ON_AT_HOME_SUM.INPT_MED(+)
	AND LA_DOSES.ENCNTR_ID = INTERMIT_MME_AVG.ENCNTR_ID(+)
	AND LA_DOSES.ENCNTR_ID = HOME_MEDS_MME_TOTAL.ENCNTR_ID(+)
	AND LA_DOSES.ENCNTR_ID = FIRST_DOSES.ENCNTR_ID(+)
	AND LA_DOSES.ITEM_ID = FIRST_DOSES.ITEM_ID(+)
	AND LA_DOSES.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1
