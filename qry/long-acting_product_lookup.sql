SELECT DISTINCT
	-- ENCOUNTER.ENCNTR_ID,
	-- pi_get_cv_display(ENCOUNTER.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
	ORDER_PRODUCT.ITEM_ID,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
	MED_IDENTIFIER.VALUE AS MED_PRODUCT
FROM
	-- CE_MED_RESULT,
	CLINICAL_EVENT,
	ENCOUNTER,
	MED_IDENTIFIER,
	ORDER_PRODUCT
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
		pi_to_gmt(
			TO_DATE(
				@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:0), 
				pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
			), 
			pi_time_zone(1, @Variable('BOUSER'))
		)
		AND pi_to_gmt(
			TO_DATE(
				@Prompt('Enter end date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:1), 
				pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
			) - 1/86400, 
			pi_time_zone(1, @Variable('BOUSER'))
		)
	-- AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
	-- AND CE_MED_RESULT.IV_EVENT_CD > 0
	AND CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
	AND ENCOUNTER.LOC_FACILITY_CD IN (
		3310, -- HH HERMANN
		3796, -- HC Childrens
		3821, -- HH Clinics
		3822, -- HH Trans Care
		3823 -- HH Rehab		
	)
	AND CLINICAL_EVENT.ORDER_ID = ORDER_PRODUCT.ORDER_ID
	AND ORDER_PRODUCT.ITEM_ID = MED_IDENTIFIER.ITEM_ID
	AND MED_IDENTIFIER.MED_IDENTIFIER_TYPE_CD = 1564 -- Description
	AND MED_IDENTIFIER.MED_PRODUCT_ID = 0