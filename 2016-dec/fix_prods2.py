import json

with open('fixed_orders.csv', 'w') as fout:
    fout.write(
        'name\tinvoice_number\tproduct_id\tproduct_name\tproduct_price\tplaced_date\n')

    with open('vendor_orders.json', 'r') as fin:
        jdata = json.loads(fin.read())

        for vorder in jdata:
            products = json.loads(vorder['products'])

            for prod in products:
                row = [vorder['name'], vorder['invoice_number'], str(prod['id']),
                       prod['name'], str(prod['price']), vorder['placed_date'].replace('"', '')]

                new_line = '\t'.join(row) + '\n'

                for i in range(prod['count']):
                    fout.write(new_line)
