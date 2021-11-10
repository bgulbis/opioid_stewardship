WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCNTR_ALIAS.ALIAS AS FIN,
		TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) - PERSON.BIRTH_DT_TM) / 365.25, 0) AS AGE,
		pi_get_cv_display(PERSON.SEX_CD) AS SEX,
		pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago') AS ADMIT_DATETIME,
		pi_from_gmt(ENCOUNTER.DISCH_DT_TM, 'America/Chicago') AS DISCH_DATETIME,
		ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM AS LOS,
		NOMENCLATURE.SOURCE_IDENTIFIER AS ICD_CODE,
		NOMENCLATURE.SOURCE_STRING
	FROM
		DIAGNOSIS,
		ENCNTR_ALIAS,
		ENCNTR_LOC_HIST,
		ENCOUNTER,
		NOMENCLATURE,
		PERSON
	WHERE
		ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
		AND ENCOUNTER.REG_DT_TM BETWEEN 
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'07/01/2017 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				pi_time_zone(1, 'America/Chicago')
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'07/01/2018 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				pi_time_zone(1, 'America/Chicago')
			)
		AND ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM >= 3
		AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
		AND ENCOUNTER.ADMIT_SRC_CD = 9061 -- Emergency Room
		AND ENCOUNTER.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		AND DIAGNOSIS.DIAG_PRIORITY = 1
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^D57') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
		AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		AND TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) - PERSON.BIRTH_DT_TM) / 365.25, 0) >= 18
		AND ENCOUNTER.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCOUNTER.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD IN (
			3861, -- HH 5WCP
			-- 3921, -- HH 5ECP
			3955, -- HH 4WCP
			8667250, -- HH MIMU
			265179687, -- HH ACE
			1846341943, -- HH 3CP
			1846342787 -- HH 3CIM
		)
), UNIT_IN_OUT AS (
	SELECT DISTINCT
		ENCNTR_LOC_HIST.ENCNTR_ID,
		ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM,
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD,
		LEAST(ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM, ENCOUNTER.DISCH_DT_TM) - ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AS TRANSACTION_DURATION,
		CASE
			WHEN 
				LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN 1
			ELSE 0
		END AS UNIT_IN
	FROM
		ENCNTR_LOC_HIST,
		ENCOUNTER,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND PATIENTS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND ENCOUNTER.DISCH_DT_TM >= ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM
), UNIT_COUNT AS (
	SELECT DISTINCT
		ENCNTR_ID,
		SUM(UNIT_IN) OVER (PARTITION BY ENCNTR_ID ORDER BY BEG_EFFECTIVE_DT_TM) AS UNIT_COUNT,
		BEG_EFFECTIVE_DT_TM,
		LOC_NURSE_UNIT_CD
	FROM
		UNIT_IN_OUT
), UNIT_FIRST AS (
	SELECT
		ENCNTR_ID,
		MIN(UNIT_COUNT) AS UNIT_COUNT,
		MIN(LOC_NURSE_UNIT_CD) KEEP (DENSE_RANK FIRST ORDER BY UNIT_COUNT) AS LOC_NURSE_UNIT_CD,
		MIN(BEG_EFFECTIVE_DT_TM) KEEP (DENSE_RANK FIRST ORDER BY UNIT_COUNT) AS BEG_EFFECTIVE_DT_TM
	FROM 
		UNIT_COUNT
	WHERE 
		LOC_NURSE_UNIT_CD NOT IN (
			277567038, -- HH VUHH
			277570335, -- HH EDHH
			277573736, -- HH EDTR
			508618957, -- HH EREV
			2146281534, -- HH OBEC
			3224989205, -- HH S VUHH
			3224989653, -- HH S EDHH
			3224990761, -- HH S EDTR
			3224990803 -- HH S EREV
		)
	GROUP BY
		ENCNTR_ID
)

SELECT
	PATIENTS.*,
	pi_get_cv_display(UNIT_FIRST.LOC_NURSE_UNIT_CD) AS ADMIT_NURSE_UNIT,
	pi_from_gmt(UNIT_FIRST.BEG_EFFECTIVE_DT_TM, 'America/Chicago') AS CULLEN_DATETIME,
	UNIT_FIRST.UNIT_COUNT
FROM
	PATIENTS,
	UNIT_FIRST
WHERE
	PATIENTS.ENCNTR_ID = UNIT_FIRST.ENCNTR_ID
	AND UNIT_FIRST.LOC_NURSE_UNIT_CD IN (
		3861, -- HH 5WCP
		3955, -- HH 4WCP
		8667250, -- HH MIMU
		265179687, -- HH ACE
		1846341943, -- HH 3CP
		1846342787 -- HH 3CIM
	)