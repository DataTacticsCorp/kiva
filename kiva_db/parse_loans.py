import psycopg2
import codecs
import json
import time
import glob
import sys

jfiles = glob.glob('/data/kiva/loans/*.json')

count = 0
errors = 0
insert = True

def clean(string):
	if string != None:
		return string.replace('\'', '').replace('\"', '').replace('\n', '\t').replace('\\', '')
	else:
		return string

def sql_date(string):
	if string != None:
		return string.replace('T', ' ').replace('Z', '+00')
	else:
		return string

if insert:
	try:
		conn = psycopg2.connect("dbname='kiva' user='postgres' host='127.0.0.1' password='abc!!123'")
	except:
		error += 1
		sys.exit(1)
	cur = conn.cursor()

print "CONNECTED!\n"

file_count = len(jfiles)
current_file = 1

loans_errors = 0
borrowers_errors = 0
description_errors = 0
payments_errors = 0
terms_errors = 0
location_errors = 0

for json_file in jfiles:

	error_log = codecs.open('ll_errors.log', 'a', 'utf-8')

	#print json_file
	jfile = codecs.open(json_file, 'r', 'utf-8')
	jdata = jfile.read()

	try:
		json_object = json.loads(jdata)
	
	except Exception, e:
		print 'FILE ERROR'
		pass

	results = json_object['loans']

	for result in json_object['loans']:
	
		#print result
	
		loan_id = result['id']
		activity = clean(result['activity'])
		basket_amount = result['basket_amount']
		currency_exchange_loss_amount = result['currency_exchange_loss_amount']
		delinquent = result['delinquent']	
		funded_amount = result['funded_amount']
		funded_date = result['funded_date']
		loan_amount = result['loan_amount']
		name = clean(result['name'])
		paid_amount = result['paid_amount']
		if paid_amount == None:
			paid_amount = 0.0
		#paid_date = result['paid_date']
		partner_id = result['partner_id']
		posted_date = result['posted_date']
		sector = clean(result['sector'])
		status = clean(result['status'])
		use = clean(result['use'])

		sql = "INSERT INTO kiva_loans(loan_id, activity, funded_amount, funded_date, loan_amount, name, paid_amount, partner_id, posted_date, sector, status, use) VALUES ("+unicode(loan_id)+", E\'"+unicode(activity)+"\', "+unicode(funded_amount)+", E\'"+unicode(funded_date)+"\', "+unicode(loan_amount)+", E\'"+unicode(name)+"\', "+unicode(paid_amount)+", "+unicode(partner_id)+", E\'"+unicode(posted_date)+"\', E\'"+unicode(sector)+"\', E\'"+unicode(status)+"\', E\'"+unicode(use)+"\');"
		#print sql
		if insert:
			try:					
				cur.execute(sql)
			except Exception, e:
				loans_errors += 1
				#print e
				pass
		if insert: conn.commit()


		borrowers = result['borrowers']
		for barrower in borrowers:
			first_name = barrower['first_name']
			gender = barrower['gender']
			last_name = barrower['last_name']
			sql = "INSERT INTO kiva_loans_borrowers(loan_id, first_name, last_name, gender) VALUES ("+unicode(loan_id)+", E\'"+unicode(first_name)+"\', E\'"+unicode(last_name)+"\', E\'"+unicode(gender)+"\');"
			if insert:
				try:					
					cur.execute(sql)
				except Exception, e:
					borrowers_errors += 1
					pass
		if insert: conn.commit()

		description = result['description']
		languages = description['languages']
		texts = description['texts']
		for language in languages:
			lang_desc = clean(texts[language])
			sql = "INSERT INTO kiva_loans_descriptions(loan_id, language, description_text) VALUES ("+unicode(loan_id)+", E\'"+unicode(language)+"\', E\'"+unicode(lang_desc)+"\');"
			if insert:
				try:					
					cur.execute(sql)
				except Exception, e:
					description_errors += 1
					pass
		if insert: conn.commit()

		payments = result['payments']
		for payment in payments:
			amount = payment['amount']
			comment = clean(payment['comment'])
			payment_cela = payment['currency_exchange_loss_amount']
			local_amount = payment['local_amount']
			payment_id = payment['payment_id']
			processed_date = sql_date(payment['processed_date'])
			settlement_date = sql_date(payment['settlement_date'])
			sql = "INSERT INTO kiva_loans_payments(loan_id, amount, comment, payment_cela, local_amount, payment_id, processed_date, settlement_date) VALUES ("+unicode(loan_id)+", "+unicode(amount)+", E\'"+unicode(comment)+"\', "+unicode(payment_cela)+", "+unicode(local_amount)+", "+unicode(payment_id)+", E\'"+unicode(processed_date)+"\', E\'"+unicode(settlement_date)+"\');"
			#print sql
			if insert:
				try:					
					cur.execute(sql)
				except Exception, e:
					payments_errors += 1
					pass
		if insert: conn.commit()


		terms = result['terms']
		disbursal_amount = terms['disbursal_amount']
		disbursal_currency = clean(terms['disbursal_currency'])
		disbursal_date = sql_date(terms['disbursal_date'])
		loan_amount = terms['loan_amount']

		sql = "INSERT INTO kiva_loans_terms(loan_id, disbursal_amount, disbursal_currency, disbursal_date, loan_amount) VALUES ("+unicode(loan_id)+", "+unicode(disbursal_amount)+", E\'"+unicode(disbursal_currency)+"\', E\'"+unicode(disbursal_date)+"\', "+unicode(loan_amount)+");"
		if insert:
			try:					
				cur.execute(sql)
			except Exception, e:
				terms_errors += 1
				pass
			conn.commit()

		local_payments = terms['local_payments']
		for local_payment in local_payments:
			amount = local_payment['amount']
			due_date = sql_date(local_payment['due_date'])
			sql = "INSERT INTO kiva_loans_terms_local_payments(loan_id, amount, due_date) VALUES ("+unicode(loan_id)+", "+unicode(amount)+", E\'"+unicode(due_date)+"\');"
			if insert:
				try:					
					cur.execute(sql)
				except Exception, e:
					terms_errors += 1
					pass
		if insert: conn.commit()

		loss_liability = terms['loss_liability']
		currency_exchange = loss_liability['currency_exchange']
		currency_exchange_coverage_rate = loss_liability['currency_exchange_coverage_rate']
		nonpayment = loss_liability['nonpayment']
		sql = "INSERT INTO kiva_loans_terms_loss_liability(loan_id, currency_exchange, currency_exchange_coverage_rate, nonpayment) VALUES ("+unicode(loan_id)+", E\'"+unicode(currency_exchange)+"\', E\'"+unicode(currency_exchange_coverage_rate)+"\', E\'"+unicode(nonpayment)+"\');"
		if insert:
			try:					
				cur.execute(sql)
			except Exception, e:
				terms_errors += 1
				pass
			conn.commit()

		scheduled_payments = terms['scheduled_payments']
		for scheduled_payment in scheduled_payments:
			amount = scheduled_payment['amount']
			due_date = sql_date(scheduled_payment['due_date'])
			sql = "INSERT INTO kiva_loans_terms_scheduled_payments(loan_id, amount, due_date) VALUES ("+unicode(loan_id)+", "+unicode(amount)+", E\'"+unicode(due_date)+"\');"
			if insert:
				try:					
					cur.execute(sql)
				except Exception, e:
					terms_errors += 1
					pass
		if insert: conn.commit()

		location = result['location']
		geo = location['geo']
		country = location['country']
		country_code = location['country_code']
		geo_level = geo['level']
		geo_pairs = geo['pairs']
		geo_lng = geo_pairs.split()[1].strip()
		geo_lat = geo_pairs.split()[0].strip()
		geo_type = geo['type']
		sql = "INSERT INTO kiva_loans_locations(loan_id, country, country_code, geo_level, geo_type, geo_lng, geo_lat, geom) VALUES ("+unicode(loan_id)+", E\'"+unicode(country)+"\', E\'"+unicode(country_code)+"\', E\'"+unicode(geo_level)+"\', E\'"+unicode(geo_type)+"\', "+unicode(geo_lng)+", "+unicode(geo_lat)+",ST_GeomFromText('POINT("+unicode(geo_lng)+" "+unicode(geo_lat)+")', 4326));"
		if insert:
			try:					
				cur.execute(sql)
			except Exception, e:
				location_errors += 1
				pass
		if insert: conn.commit()

		count += 1

	print str(current_file)+"/"+str(file_count)+"\t\tCOUNT: "+str(count)+"\tERRORS:\tLE: "+str(loans_errors)+"  BE: "+str(borrowers_errors)+"  DE: "+str(description_errors)+"  PE: "+str(payments_errors)+"  GE: "+str(location_errors)+"  TE: "+str(terms_errors)
	current_file += 1


'''

DROP TABLE kiva_loans;

CREATE TABLE kiva_loans
(
  loan_id bigint NOT NULL,
  activity character varying(1000),
  funded_amount double precision,
  funded_date timestamp with time zone,
  loan_amount double precision,
  name character varying(128),
  paid_amount double precision,
  partner_id bigint,
  posted_date timestamp with time zone,
  sector character varying(1000),
  status character varying(128),
  use character varying(1000)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loans
  OWNER TO postgres;

DROP TABLE kiva_loans_borrowers;

CREATE TABLE kiva_loans_borrowers
(
  loan_id bigint NOT NULL,
  first_name character varying(128),
  last_name character varying(128),
  gender character varying(128)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loans_borrowers
  OWNER TO postgres;

DROP TABLE kiva_loans_descriptions;

CREATE TABLE kiva_loans_descriptions
(
  loan_id bigint NOT NULL,
  language character varying(100),
  description_text text
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loans_descriptions
  OWNER TO postgres;

DROP TABLE kiva_loans_locations;

CREATE TABLE kiva_loans_locations
(
  loan_id bigint NOT NULL,
  country character varying(128),
  country_code character varying(20),
  geo_level character varying(128),
  geo_type character varying(128),
  geo_lng double precision,
  geo_lat double precision,
  geom geometry
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loans_locations
  OWNER TO postgres;

DROP TABLE kiva_loans_payments;

CREATE TABLE kiva_loans_payments
(
  loan_id bigint NOT NULL,
  amount double precision,
  comment text,
  payment_cela double precision,
  local_amount double precision,
  payment_id bigint,
  processed_date timestamp with time zone,
  settlement_date timestamp with time zone
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loans_payments
  OWNER TO postgres;

DROP TABLE kiva_loans_terms;

CREATE TABLE kiva_loans_terms
(
  loan_id bigint NOT NULL,
  disbursal_amount double precision,
  disbursal_currency character varying(128),
  disbursal_date timestamp with time zone,
  loan_amount double precision
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loans_terms
  OWNER TO postgres;

DROP TABLE kiva_loans_terms_local_payments;

CREATE TABLE kiva_loans_terms_local_payments
(
  loan_id bigint NOT NULL,
  amount double precision,
  due_date timestamp with time zone
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loans_terms_local_payments
  OWNER TO postgres;

DROP TABLE kiva_loans_terms_loss_liability;

CREATE TABLE kiva_loans_terms_loss_liability
(
  loan_id bigint NOT NULL,
  currency_exchange character varying(128),
  currency_exchange_coverage_rate character varying(128),
  nonpayment character varying(128)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loans_terms_loss_liability
  OWNER TO postgres;

DROP TABLE kiva_loans_terms_scheduled_payments;

CREATE TABLE kiva_loans_terms_scheduled_payments
(
  loan_id bigint NOT NULL,
  amount double precision,
  due_date timestamp with time zone
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loans_terms_scheduled_payments
  OWNER TO postgres;

'''


