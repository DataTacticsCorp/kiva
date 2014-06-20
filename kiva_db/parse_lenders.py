import psycopg2
import codecs
import json
import time
import glob
import sys

jfiles = glob.glob('/data/kiva/lenders/*.json')

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
		print "I am unable to connect to the database"
		sys.exit(1)
	cur = conn.cursor()

print "CONNECTED!\n"

for json_file in jfiles:

	print 'F',
	
	error_log = codecs.open('lenders_errors.log', 'a', 'utf-8')

	jfile = codecs.open(json_file, 'r', 'utf-8')
	jdata = jfile.read()

	try:
		json_object = json.loads(jdata)
	except Exception, e:
		#error_log.write("\n\nERROR: "+unicode(e)+"\n\n")
		errors += 1
		print 'L',

	results = json_object['lenders']

	for result in json_object['lenders']:

		country_code = None
		invitee_count = None
		inviter_id = None
		lender_id = None
		loan_because = None
		loan_count = None
		member_since = None
		name = None
		occupation = None
		occupational_info = None
		personal_url = None
		uid = None
		whereabouts = None

		country_code = result['country_code']
		invitee_count = result['invitee_count']
		inviter_id = result['inviter_id']
		lender_id = result['lender_id']
		loan_because = clean(result['loan_because'])
		loan_count = result['loan_count']
		member_since = sql_date(result['member_since'])
		name = clean(result['name'])
		occupation = clean(result['occupation'])
		occupational_info = clean(result['occupational_info'])
		personal_url = clean(result['personal_url'])
		uid = result['uid']
		whereabouts = clean(result['whereabouts'])

		#print country_code+','+str(invitee_count)+','+inviter_id+','+lender_id+','+loan_because+','+str(loan_count)+','+member_since+','+name+','+occupation+','+occupational_info+','+personal_url+','+uid+','+whereabouts

		sql = "INSERT INTO kiva_lenders(country_code, invitee_count, inviter_id, loan_because, loan_count, member_since, name, occupation, occupational_info, personal_url, uid, whereabouts) VALUES (E\'"+unicode(country_code)+"\', "+unicode(invitee_count)+", E\'"+unicode(inviter_id)+"\', E\'"+unicode(loan_because)+"\', "+unicode(loan_count)+", E\'"+unicode(member_since)+"\', E\'"+unicode(name)+"\', E\'"+unicode(occupation)+"\', E\'"+unicode(occupational_info)+"\', E\'"+unicode(personal_url)+"\', E\'"+unicode(uid)+"\', E\'"+unicode(whereabouts)+"\');"

		if insert:
			try:
				cur.execute(sql)
				count += 1
			except Exception, e:
				errors += 1
			#	print e
				print 'S',
				pass
		else:
			print sql
	conn.commit()

print 'COUNT:\t'+str(count)+'\tERRORS:\t'+str(errors)


'''

DROP TABLE kiva_lenders;

CREATE TABLE kiva_lenders
(
  id serial NOT NULL,
  country_code character varying(10),
  invitee_count bigint,
  inviter_id character varying(128),
  loan_because text,
  loan_count bigint,
  member_since timestamp with time zone,
  name character varying(128),
  occupation character varying(256),
  occupational_info text,
  personal_url character varying(512),
  uid character varying(128),
  whereabouts character varying(128)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_lenders
  OWNER TO postgres;

'''


