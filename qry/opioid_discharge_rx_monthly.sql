WITH DC_RX AS (
	SELECT DISTINCT
		ORDERS.ORDER_ID,
		ENCOUNTER.ENCNTR_ID,
		TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) AS AGE,
		ORDERS.CATALOG_CD,
		ORDERS.ORDERED_AS_MNEMONIC,
		ORDERS.ORIG_ORDER_DT_TM,
		ENCOUNTER.LOC_FACILITY_CD,
		ENCOUNTER.LOC_NURSE_UNIT_CD,
		ENCOUNTER.MED_SERVICE_CD,
		ENCOUNTER.ENCNTR_TYPE_CLASS_CD,
		PRSNL.PERSON_ID,
		PRSNL.NAME_FULL_FORMATTED,
		PRSNL.POSITION_CD, 
		PRSNL.USERNAME,
		PRSNL_ALIAS.ALIAS AS PROVIDER_NPI,
		FREQUENCY_SCHEDULE.FREQUENCY_CD,
		ORDERS.PRN_IND
	FROM
		ENCOUNTER,
		FREQUENCY_SCHEDULE,
		ORDER_ACTION,
		ORDERS,
		PERSON,
		PI_THERA_CLASS_VIEW,
		PRSNL,
		PRSNL_ALIAS
	WHERE
		PI_THERA_CLASS_VIEW.DRUG_CAT IN (
			'narcotic analgesic combinations', 
			'narcotic analgesics'
		)
		AND PI_THERA_CLASS_VIEW.DRUG_CAT_CD = ORDERS.CATALOG_CD
		AND ORDERS.CATALOG_CD NOT IN (
			9909955, -- belladonna-opium
			9909879, -- APAP/butalbital/caffeine/codeine
			9909887 -- ASA/butalbital/caffeine/codeine
		)
		AND ORDERS.ORIG_ORDER_DT_TM BETWEEN
			DECODE(
				@Prompt('Choose date range', 'A', {'Last Week', 'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), 'America/Chicago'),
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
				@Prompt('Choose date range', 'A', {'Last Week', 'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, 'America/Chicago'),
				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - 1/86400, 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date', 'D', , mono, free, persistent, {'01/01/2021 00:00:00'}, User:2),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					) - 1/86400,
					pi_time_zone(1, 'America/Chicago')
				)
			)
		AND ORDERS.ORIG_ORD_AS_FLAG = 1 -- Prescription/Discharge Order
		AND ORDERS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND (
			ENCOUNTER.LOC_FACILITY_CD IN (
				3310, -- HH HERMANN
				-- 3796, -- HC Childrens
				-- 3821, -- HH Clinics
				3822, -- HH Trans Care
				3823 -- HH Rehab
			)
			OR ENCOUNTER.LOC_NURSE_UNIT_CD IN (
				43683728, -- HH DSU
				43683126, -- HH AMSA
				3245580207 -- HH S AMSA
			)
		)
		AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		AND ORDERS.ORDER_ID = ORDER_ACTION.ORDER_ID
		AND ORDER_ACTION.ACTION_TYPE_CD = 1376 -- Order
		AND ORDER_ACTION.ORDER_PROVIDER_ID = PRSNL.PERSON_ID
		AND PRSNL.PERSON_ID = PRSNL_ALIAS.PERSON_ID
		AND PRSNL_ALIAS.PRSNL_ALIAS_TYPE_CD = 96412573 -- National Provider Identifier
		AND ORDERS.FREQUENCY_ID = FREQUENCY_SCHEDULE.FREQUENCY_ID
), LAST_RX AS (
	SELECT DISTINCT
		MAX(DC_RX.ORDER_ID) AS ORDER_ID,
		DC_RX.ENCNTR_ID,
		CASE
			WHEN DC_RX.AGE < 18 THEN '0-17'
			WHEN DC_RX.AGE < 65 THEN '18-64'
			ELSE '65+'
		END AS AGE_GROUP,
		DC_RX.CATALOG_CD,
		MAX(DC_RX.ORDERED_AS_MNEMONIC) KEEP (DENSE_RANK LAST ORDER BY DC_RX.ORDER_ID) AS ORDERED_AS_MNEMONIC,
		MAX(DC_RX.ORIG_ORDER_DT_TM) AS ORIG_ORDER_DT_TM,
		DC_RX.LOC_FACILITY_CD,
		DC_RX.LOC_NURSE_UNIT_CD,
		DC_RX.MED_SERVICE_CD,
		DC_RX.ENCNTR_TYPE_CLASS_CD,
		MAX(DC_RX.PERSON_ID) KEEP (DENSE_RANK LAST ORDER BY DC_RX.ORDER_ID) AS PERSON_ID,
		MAX(DC_RX.NAME_FULL_FORMATTED) KEEP (DENSE_RANK LAST ORDER BY DC_RX.ORDER_ID) AS NAME_FULL_FORMATTED,
		MAX(DC_RX.POSITION_CD) KEEP (DENSE_RANK LAST ORDER BY DC_RX.ORDER_ID) AS POSITION_CD,
		MAX(DC_RX.USERNAME) KEEP (DENSE_RANK LAST ORDER BY DC_RX.ORDER_ID) AS USERNAME,
		MAX(DC_RX.PROVIDER_NPI) KEEP (DENSE_RANK LAST ORDER BY DC_RX.ORDER_ID) AS PROVIDER_NPI,
		MAX(DC_RX.FREQUENCY_CD) KEEP (DENSE_RANK LAST ORDER BY DC_RX.ORDER_ID) AS FREQUENCY_CD,
		MAX(DC_RX.PRN_IND) KEEP (DENSE_RANK LAST ORDER BY DC_RX.ORDER_ID) AS PRN_IND
	FROM
		DC_RX
	GROUP BY
		DC_RX.ENCNTR_ID,
		DC_RX.AGE,
		DC_RX.CATALOG_CD,
		DC_RX.LOC_FACILITY_CD,
		DC_RX.LOC_NURSE_UNIT_CD,
		DC_RX.MED_SERVICE_CD,
		DC_RX.ENCNTR_TYPE_CLASS_CD			
), MD_DEA AS (
	SELECT DISTINCT
		LAST_RX.ORDER_ID,
		MAX(PRSNL_ALIAS.ALIAS) KEEP (DENSE_RANK LAST ORDER BY PRSNL_ALIAS.BEG_EFFECTIVE_DT_TM) AS PROVIDER_DEA
	FROM
		LAST_RX,
		PRSNL_ALIAS
	WHERE
		LAST_RX.PERSON_ID = PRSNL_ALIAS.PERSON_ID
		AND PRSNL_ALIAS.PRSNL_ALIAS_TYPE_CD = 625 -- DOCDEA
		AND PRSNL_ALIAS.BEG_EFFECTIVE_DT_TM <= LAST_RX.ORIG_ORDER_DT_TM
	GROUP BY
		LAST_RX.ORDER_ID
), ORD_DETAILS AS (
	SELECT DISTINCT
		ORDER_DETAIL.ORDER_ID,
		ORDER_DETAIL.OE_FIELD_MEANING,
		ORDER_DETAIL.OE_FIELD_DISPLAY_VALUE
	FROM
		LAST_RX,
		ORDER_DETAIL
	WHERE
		LAST_RX.ORDER_ID = ORDER_DETAIL.ORDER_ID
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
			'REQROUTINGTYPE' AS ROUTING,
			'RXROUTE' AS ROUTE,
			'SPECINX' AS SPECIAL_INSTR,
			'STRENGTHDOSE' AS DOSE,
			'STRENGTHDOSEUNIT' AS DOSE_UNIT,
			'VOLUMEDOSE' AS VOLUME,
			'VOLUMEDOSEUNIT' AS VOLUME_UNIT
		)
	)
), ATTENDINGS_INPT AS (
	SELECT
		LAST_RX.ENCNTR_ID,
		LAST_RX.ORDER_ID,
		ENCNTR_PRSNL_RELTN.ENCNTR_PRSNL_RELTN_ID,
		PRSNL.NAME_FULL_FORMATTED,
		PRSNL.POSITION_CD,
		pi_get_cv_display(PRSNL.POSITION_CD) AS POSITION
	FROM
		ENCNTR_PRSNL_RELTN,
		LAST_RX,
		PRSNL
	WHERE
		LAST_RX.ENCNTR_ID = ENCNTR_PRSNL_RELTN.ENCNTR_ID
		AND LAST_RX.ENCNTR_TYPE_CLASS_CD <> 55851 -- Emergency
		AND ENCNTR_PRSNL_RELTN.ENCNTR_PRSNL_R_CD = 368029 -- Physician Attending
		AND ENCNTR_PRSNL_RELTN.ACTIVE_IND = 1
		AND LAST_RX.ORIG_ORDER_DT_TM BETWEEN 
			ENCNTR_PRSNL_RELTN.BEG_EFFECTIVE_DT_TM 
			AND ENCNTR_PRSNL_RELTN.END_EFFECTIVE_DT_TM
		AND ENCNTR_PRSNL_RELTN.TRANSACTION_DT_TM = (
			SELECT MAX(EPR.TRANSACTION_DT_TM)
			FROM ENCNTR_PRSNL_RELTN EPR
			WHERE EPR.TRANSACTION_DT_TM <= LAST_RX.ORIG_ORDER_DT_TM
				AND LAST_RX.ENCNTR_ID = EPR.ENCNTR_ID
				AND EPR.ENCNTR_PRSNL_R_CD = 368029 -- Physician Attending
				AND EPR.ACTIVE_IND = 1
		)
		AND ENCNTR_PRSNL_RELTN.PRSNL_PERSON_ID = PRSNL.PERSON_ID
), ATTENDINGS_ED AS (
	SELECT
		LAST_RX.ENCNTR_ID,
		LAST_RX.ORDER_ID,
		ENCNTR_PRSNL_RELTN.ENCNTR_PRSNL_RELTN_ID,
		PRSNL.NAME_FULL_FORMATTED,
		PRSNL.POSITION_CD,
		pi_get_cv_display(PRSNL.POSITION_CD) AS POSITION
	FROM
		ENCNTR_PRSNL_RELTN,
		LAST_RX,
		PRSNL
	WHERE
		LAST_RX.ENCNTR_ID = ENCNTR_PRSNL_RELTN.ENCNTR_ID
		AND LAST_RX.ENCNTR_TYPE_CLASS_CD = 55851 -- Emergency
		AND ENCNTR_PRSNL_RELTN.ENCNTR_PRSNL_R_CD = 294583049 -- ED Physician
		AND ENCNTR_PRSNL_RELTN.ACTIVE_IND = 1
		AND LAST_RX.ORIG_ORDER_DT_TM BETWEEN 
			ENCNTR_PRSNL_RELTN.BEG_EFFECTIVE_DT_TM 
			AND ENCNTR_PRSNL_RELTN.END_EFFECTIVE_DT_TM
		AND ENCNTR_PRSNL_RELTN.TRANSACTION_DT_TM = (
			SELECT MAX(EPR.TRANSACTION_DT_TM)
			FROM ENCNTR_PRSNL_RELTN EPR
			WHERE EPR.TRANSACTION_DT_TM <= LAST_RX.ORIG_ORDER_DT_TM
				AND LAST_RX.ENCNTR_ID = EPR.ENCNTR_ID
				AND EPR.ENCNTR_PRSNL_R_CD = 294583049 -- ED Physician
				AND EPR.ACTIVE_IND = 1
		)
		AND ENCNTR_PRSNL_RELTN.PRSNL_PERSON_ID = PRSNL.PERSON_ID
), ATTENDINGS AS (
	SELECT * FROM ATTENDINGS_INPT
	UNION
	SELECT * FROM ATTENDINGS_ED
), HOSPITALISTS AS (
	SELECT 
		PERSON_ID, 
		NAME_FULL_FORMATTED
	FROM PRSNL
	WHERE 
		PRSNL.PERSON_ID IN (
			100202005, -- Thomas, Nithin Georgey MD
			102714538, -- Hussain, Zoya DO
			103507408, -- Kang, Julian J DO
			105357863, -- Sreedhar, Arsha Nambiar MD
			106610654, -- Karian, Dharma MD
			106985087, -- Mathew, Susan Vijayan DO
			107755517, -- Akpo, Victorine L
			108163768, -- Varughese, Kunjumol M
			108767305, -- Pearson, Melissa D
			109007617, -- Wesonga, Caroline A  ACNP
			44743162, -- Khraish, Gina Hanna MD
			55359789, -- Harbison, Matthew Trevor MD
			60851996, -- Xavier, Andreea Simona MD
			61311669, -- Thomas, Jason Joseph MD
			61983529, -- Bubis, Michael Hayden DO
			62035819, -- Plessner, Melissa DO
			62036813, -- Reichert, Sarah Irene DO
			77647330, -- Amin, Amee M MD
			79344681, -- Mouser, Benjamin D MD
			83246004, -- Boyars, Michael Eugene MD
			83993557, -- Schindler, Adam Michael MD
			84352684, -- Wang, Daniel Vincent MD
			87353341, -- Pan, Zan MD
			89054017, -- Shah, Priti Ashok DO
			89054325, -- Patel, Napoleon Kishorkumar MD
			89309962, -- Onyema, Ezenwa Chizimako MD
			89346967, -- Bishop, Ivy
			90072414, -- Okere, Jacqueline Ugonna MD
			91826257, -- Garg, Anjali MD
			93519686, -- Bruera, Sebastian MD
			93654091, -- Clark, Benjamin J MD
			94425003, -- Cala Noriega, Jose A MD
			99028366, -- Abraham, Shifa MD
			99047131, -- Tiwari, Ritik MD
			99091369, -- Jaynes, Jennifer Michelle DO
			99140184 -- Mossaad, Afnan Karam MD
		)
), RX_ORDERS AS (
	SELECT DISTINCT
        LAST_RX.ORDER_ID,
        pi_from_gmt(LAST_RX.ORIG_ORDER_DT_TM, 'America/Chicago') AS ORDER_DATETIME,
        TRUNC(pi_from_gmt(LAST_RX.ORIG_ORDER_DT_TM, 'America/Chicago'), 'MONTH') AS ORDER_MONTH,
        LAST_RX.ENCNTR_ID,
        LAST_RX.AGE_GROUP,
        LOWER(pi_get_cv_display(LAST_RX.CATALOG_CD)) AS MEDICATION,
        LOWER(LAST_RX.ORDERED_AS_MNEMONIC) AS ORDERED_AS,
        TO_NUMBER(REGEXP_REPLACE(ORD_DETAIL_PIVOT.DOSE, '[^0-9.]', '')) AS DOSE,
        LOWER(ORD_DETAIL_PIVOT.DOSE_UNIT) AS DOSE_UNIT,
        TO_NUMBER(REGEXP_REPLACE(ORD_DETAIL_PIVOT.VOLUME, '[^0-9.]', '')) AS VOLUME,
        LOWER(ORD_DETAIL_PIVOT.VOLUME_UNIT) AS VOLUME_UNIT,
        LOWER(ORD_DETAIL_PIVOT.FREETXT_DOSE) AS FREETXT_DOSE,
        REGEXP_REPLACE(LOWER(ORD_DETAIL_PIVOT.FREETXT_DOSE), '[[:space:]]', '') AS FREETXT_DOSE_NOSPACE,
        LOWER(ORD_DETAIL_PIVOT.ROUTE) AS ROUTE,
        LOWER(pi_get_cv_display(LAST_RX.FREQUENCY_CD)) AS FREQ,
        CASE LAST_RX.PRN_IND
            WHEN 1 THEN 'PRN'
            ELSE 'Scheduled'
        END AS PRN,
        LOWER(ORD_DETAIL_PIVOT.PRN_INSTR) AS PRN_INSTR,
        LOWER(ORD_DETAIL_PIVOT.SPECIAL_INSTR) AS SPECIAL_INSTR,
        REGEXP_REPLACE(LOWER(ORD_DETAIL_PIVOT.SPECIAL_INSTR), '[[:space:]]', '') AS SPECIAL_INSTR_NOSPACE,
        TO_NUMBER(REGEXP_REPLACE(ORD_DETAIL_PIVOT.DISPENSE_QTY, '[^0-9.]', '')) AS DISPENSE_QTY,
        ORD_DETAIL_PIVOT.DISPENSE_UNIT AS DISPENSE_UNIT,
        TO_NUMBER(REGEXP_REPLACE(ORD_DETAIL_PIVOT.DURATION, '[^0-9.]', '')) AS DURATION,
        ORD_DETAIL_PIVOT.DURATION_UNIT AS DURATION_UNIT,
        TO_NUMBER(REGEXP_REPLACE(ORD_DETAIL_PIVOT.REFILLS, '[^0-9.]', '')) AS REFILLS,
        ORD_DETAIL_PIVOT.ROUTING,
        LAST_RX.NAME_FULL_FORMATTED AS PROVIDER,
        pi_get_cv_display(LAST_RX.POSITION_CD) AS PROVIDER_POSITION,
        LAST_RX.USERNAME,
        LAST_RX.PROVIDER_NPI,
        MD_DEA.PROVIDER_DEA,
        pi_get_cv_display(LAST_RX.MED_SERVICE_CD) AS MED_SERVICE,
        pi_get_cv_display(LAST_RX.LOC_FACILITY_CD) AS FACILITY,
        pi_get_cv_display(LAST_RX.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
        pi_get_cv_display(LAST_RX.ENCNTR_TYPE_CLASS_CD) AS ENCNTR_TYPE,
        CASE
            WHEN HOSPITALISTS.PERSON_ID > 0 THEN 'Yes'
            ELSE 'No'
        END AS HOSPITALIST,
        CASE
            WHEN pi_get_cv_display(LAST_RX.POSITION_CD) = 'MD Hospitalist PB eOrder' THEN 'Yes'
            ELSE 'No'
        END AS HOSP_BY_POSN,
        ATTENDINGS.NAME_FULL_FORMATTED AS ATTENDING,
        ATTENDINGS.POSITION AS ATTENDING_POSITION
	FROM
		ATTENDINGS,
		HOSPITALISTS,
		LAST_RX,
		MD_DEA,
		ORD_DETAIL_PIVOT
	WHERE
		LAST_RX.ORDER_ID = ORD_DETAIL_PIVOT.ORDER_ID(+)
		AND LAST_RX.PERSON_ID = HOSPITALISTS.PERSON_ID(+)
		AND LAST_RX.ORDER_ID = MD_DEA.ORDER_ID(+)
		AND LAST_RX.ORDER_ID = ATTENDINGS.ORDER_ID	
), RX_ORDERS_MOD AS (
	SELECT
		RX_ORDERS.*,
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
		END AS STRENGTH,
		CASE
			WHEN VOLUME IS NOT NULL THEN VOLUME
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
					WHEN ROUTE = 'top' THEN 7.2
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
					WHEN ROUTE = 'top' THEN 12.6
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
		RX_ORDERS
)

SELECT
	RX_ORDERS_MOD.*,
	CASE 
		WHEN DOSE_UNIT = 'mg' THEN DOSE * MME_CONVERSION * ADMIN_PER_DAY
		ELSE STRENGTH * AMOUNT * MME_CONVERSION * ADMIN_PER_DAY
	END AS MME_PER_DAY,
	-- DISPENSE_QTY / AMOUNT AS DOSES_PRESCRIBED,
	CASE
		WHEN DURATION_UNIT = 'day' THEN DURATION
		WHEN DURATION_UNIT = 'week' THEN DURATION * 7
		ELSE (DISPENSE_QTY / AMOUNT) / ADMIN_PER_DAY
	END AS DAYS_SUPPLY,
	ENCNTR_ALIAS.ALIAS AS FIN
FROM
	ENCNTR_ALIAS,
	RX_ORDERS_MOD
WHERE
	RX_ORDERS_MOD.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1
