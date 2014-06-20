import psycopg2
import codecs
import json
import time
import glob
import sys

jfiles = glob.glob('/data/kiva/loans_lenders/*.json')

for json_file in jfiles:

	insert = True

	error_log = codecs.open('ll_errors.log', 'a', 'utf-8')

	#print json_file
	jfile = codecs.open(json_file, 'r', 'utf-8')
	jdata = jfile.read()

	try:
		json_object = json.loads(jdata)
	
	except Exception, e:
		error_log.write("\n\nERROR: "+unicode(e)+"\n\n")

	results = json_object['loans_lenders']

	try:
		conn = psycopg2.connect("dbname='kiva' user='postgres' host='127.0.0.1' password='abc!!123'")
	except:
		print "I am unable to connect to the database"
		sys.exit(1)

	cur = conn.cursor()

	for result in json_object['loans_lenders']:
		loan_id = result['id']
		try:
			for lender in result['lender_ids']:
				if lender:
					sql = "INSERT INTO kiva_loan_lenders(loan_id, lender_id) VALUES ("+unicode(loan_id)+", E\'"+lender+"\');"
					if insert:
						try:					
							cur.execute(sql)
							conn.commit()
						except Exception, e:
							error_log.write("EXCEPTION:   "+unicode(e)+"\n")
							error_log.write("SLQ:   "+sql+"\n")
	      						continue
					else:
						print sql
						print str(loan_id)+'\t'+str(x)

		except Exception, e:
			print '.',
			#print 'ERROR: '+str(result['lender_ids'])
			#exit(1)
			pass

'''

DROP TABLE kiva_loan_lenders;

CREATE TABLE kiva_loan_lenders
(
  id serial NOT NULL,
  loan_id bigint NOT NULL,
  lender_id character varying(128) NOT NULL
)
WITH (
  OIDS=FALSE
);
ALTER TABLE kiva_loan_lenders
  OWNER TO postgres;

'''

