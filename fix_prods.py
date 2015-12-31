import json
import re

with open('fixed_orders.csv', 'w') as fout:
    fout.write('user_id\tinvoice_number\tproduct_id\tproduct_name\tproduct_price\tplaced_date\n')
    with open('vendor_orders.json', 'r') as fin:
        for line in fin:
            #Find user id
            m = re.search('"user_id":(\d+),', line)
            user_id = str(m.group(1))

            #Find invoice number
            m = re.search('"invoice_number":"(\d+)"', line)
            invoice_id = str(m.group(1))

            #Find the placed date
            m = re.search('(\d+-\d+-\d+)', line)
            placed_date = m.group(1)

            #Find the product JSON
            start = line.find('"products"') + len('"products":"')
            end = line.rfind('}]') + 2
            products = json.loads(line[start:end].replace('\\\\', ''))

            for prod in products:
                count = prod['count']

                rows = [user_id, invoice_id, str(prod['id']), prod['name'], str(prod['price']), placed_date]
                new_line = '\t'.join(rows) + '\n'

                for i in range(count):
                    fout.write(new_line)
