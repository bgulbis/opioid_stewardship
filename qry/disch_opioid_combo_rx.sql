WITH DISCHARGES AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCOUNTER.ENCNTR_TYPE_CLASS_CD,
		ENCOUNTER.LOC_FACILITY_CD,
		ENCOUNTER.LOC_NURSE_UNIT_CD,
		ENCOUNTER.MED_SERVICE_CD,
		TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) AS AGE
	FROM
		ENCOUNTER,
		PERSON
	WHERE
		ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
		AND ENCOUNTER.DISCH_DT_TM BETWEEN
			DECODE(
				@Prompt('Choose date range', 'A', {'Last Week', 'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), 'America/Chicago'),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/2024 00:00:00'}, User:1),
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
						@Prompt('Enter end date', 'D', , mono, free, persistent, {'02/01/2024 00:00:00'}, User:2),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					) - 1/86400,
					pi_time_zone(1, 'America/Chicago')
				)
			)		
		AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
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
), DC_RX AS (
	SELECT DISTINCT
		DISCHARGES.ENCNTR_ID,
		-- ORDERS.ORDER_ID,
		-- ORDERS.CATALOG_CD,
		pi_get_cv_display(ORDERS.CATALOG_CD) AS MEDICATION,
		CASE
			WHEN pi_get_cv_display(ORDERS.CATALOG_CD) IN ('gabapentin', 'pregabalin') THEN 'gabapentinoid'
			WHEN pi_get_cv_display(ORDERS.CATALOG_CD) IN ('diazepam', 'LORazepam', 'ALPRAZOLam', 'clonazePAM', 'temazepam') THEN 'benzo'
			WHEN pi_get_cv_display(ORDERS.CATALOG_CD) IN ('cyclobenzaprine', 'methocarbamol', 'tizanidine') THEN 'muscle_relaxant'
			ELSE 'opioid'
		END AS MED_CLASS
		-- ORDERS.ORDERED_AS_MNEMONIC
		-- ORDERS.ORIG_ORDER_DT_TM,
		-- pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, 'America/Chicago') AS ORDER_DATETIME,
		-- PRSNL.PERSON_ID AS PRSNL_PERSON_ID,
		-- PRSNL.NAME_FULL_FORMATTED,
		-- PRSNL.POSITION_CD, 
		-- PRSNL.USERNAME,
		-- PRSNL_ALIAS.ALIAS AS PROVIDER_NPI,
		-- FREQUENCY_SCHEDULE.FREQUENCY_CD,
		-- ORDERS.PRN_IND
	FROM
		DISCHARGES,
		-- FREQUENCY_SCHEDULE,
		-- ORDER_ACTION,
		ORDERS
		-- PRSNL
		-- PRSNL_ALIAS
	WHERE
		DISCHARGES.AGE >= 65
		AND DISCHARGES.ENCNTR_ID = ORDERS.ENCNTR_ID
		AND DISCHARGES.PERSON_ID = ORDERS.PERSON_ID
		AND ORDERS.CATALOG_CD IN (
			9909868, -- acetaminophen-codeine
			9909892, -- acetaminophen-hydrocodone
			9909921, -- acetaminophen-oxycodone
			9910785, -- acetaminophen-pentazocine
			15269066, -- acetaminophen-tramadol
			9903428, -- alfentanil
			9912780, -- APAP/caffeine/dihydrocodeine
			9909916, -- ASA/caffeine/dihydrocodeine
			9909949, -- ASA/caffeine/propoxyphene
			9909932, -- aspirin-oxycodone
			9910788, -- aspirin-pentazocine
			9905702, -- buprenorphine
			9905693, -- butorphanol
			9902660, -- codeine
			9910089, -- droperidol-fentanyl
			9903801, -- fentaNYL
			9912637, -- HYDROcodone-ibuprofen
			9903893, -- hydromorphone
			9905683, -- levorphanol
			9902697, -- meperidine
			9909938, -- meperidine-promethazine
			9902901, -- methadone
			9904079, -- morphine Sulfate
			9905698, -- nalbuphine
			9910761, -- naloxone-pentazocine
			9905679, -- opium
			9904177, -- oxyCODONE
			9904200, -- pentazocine
			9912058, -- remifentanil
			9904408, -- SUFentanil
			9911458, -- tramadol
			95874754, -- morphine liposomal
			99262220, -- fentaNYL-ropivacaine
			99244930, -- bupivacaine-fentaNYL
			99245019, -- bupivacaine-hydromorphone
			99245042, -- buprenorphine-naloxone
			118556266, -- dihydrocodeine
			118565850, -- HYDROcodone
			120632978, -- ibuprofen-oxycodone
			118577069, -- oxymorphone
			396857319, -- tapentadol
			538590475, -- ropivacaine-sufentanil
			1651062805, -- HYDROmorphone-ropivacaine
			2180033606, -- bupivacaine-SUFentanil
			3135779551, -- acetaminophen-benzhydrocodone
			3674350555, -- oliceridine
			4040022111, -- celecoxib-tramadol
			9908184, -- gabapentin
			85747926, -- pregabalin
			9903378, -- diazepam
			9903385, -- LORazepam
			9903431, -- ALPRAZOLam
			9903585, -- clonazePAM
			9904422, -- temazepam
			9905933, -- cyclobenzaprine
			9905939, -- methocarbamol
			9912275 -- tizanidine
		)
		AND ORDERS.CATALOG_TYPE_CD = 1363 -- Pharmacy
		AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)
		AND ORDERS.ORIG_ORD_AS_FLAG = 1 -- Prescription/Discharge Order
		-- AND ORDERS.ORDER_ID = ORDER_ACTION.ORDER_ID
		-- AND ORDER_ACTION.ACTION_TYPE_CD = 1376 -- Order
		-- AND ORDER_ACTION.ORDER_PROVIDER_ID = PRSNL.PERSON_ID
		-- AND PRSNL.PERSON_ID = PRSNL_ALIAS.PERSON_ID
		-- AND PRSNL_ALIAS.PRSNL_ALIAS_TYPE_CD = 96412573 -- National Provider Identifier
		-- AND ORDERS.FREQUENCY_ID = FREQUENCY_SCHEDULE.FREQUENCY_ID
), RX_PIVOT AS (
	SELECT * FROM DC_RX
	PIVOT(
		MIN(MEDICATION) FOR MED_CLASS IN (
			'opioid' AS OPIOID,
			'gabapentinoid' AS GABAPENTINOID,
			'benzo' AS BENZO,
			'muscle_relaxant' AS MUSCLE_RELAX
		)
	)
), PRESCRIPTS AS (
	SELECT DISTINCT
		DISCHARGES.ENCNTR_ID,
		ENCNTR_ALIAS.ALIAS AS FIN,
		DISCHARGES.AGE,
		pi_get_cv_display(DISCHARGES.ENCNTR_TYPE_CLASS_CD) AS ENCNTR_TYPE,
		pi_get_cv_display(DISCHARGES.LOC_FACILITY_CD) AS FACILITY,
		pi_get_cv_display(DISCHARGES.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
		pi_get_cv_display(DISCHARGES.MED_SERVICE_CD) AS MED_SERVICE,
		RX_PIVOT.OPIOID,
		RX_PIVOT.GABAPENTINOID,
		RX_PIVOT.BENZO,
		RX_PIVOT.MUSCLE_RELAX,
		CASE WHEN RX_PIVOT.OPIOID IS NOT NULL THEN 1 ELSE 0 END AS OPIOID_RX,
		CASE WHEN RX_PIVOT.GABAPENTINOID IS NOT NULL THEN 1 ELSE 0 END AS GABAP_RX,
		CASE WHEN RX_PIVOT.BENZO IS NOT NULL THEN 1 ELSE 0 END AS BENZO_RX,
		CASE WHEN RX_PIVOT.MUSCLE_RELAX IS NOT NULL THEN 1 ELSE 0 END AS RELAX_RX
	FROM
		DISCHARGES,
		ENCNTR_ALIAS,
		RX_PIVOT
	WHERE
		RX_PIVOT.ENCNTR_ID = DISCHARGES.ENCNTR_ID
		AND RX_PIVOT.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
)

SELECT *
FROM PRESCRIPTS
WHERE OPIOID_RX + GABAP_RX + BENZO_RX + RELAX_RX >= 2

