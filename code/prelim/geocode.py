import csv, os, time
import geocoder

known = set()

source = 'arcgis' #'geonames'

if os.path.exists("localities-" + source + ".csv"):
    with open("localities-" + source + ".csv", 'r') as fp:
        reader = csv.reader(fp)
        header = next(reader)
        for row in reader:
            known.add((row[0], row[1]))

    outfp = open("localities-" + source + ".csv", 'a')
    writer = csv.writer(outfp)
else:
    outfp = open("localities-" + source + ".csv", 'w')
    writer = csv.writer(outfp)
    if source == 'geonames':
        writer.writerow(["country", "localities", "geonames_id", "address", "description", "g_country", "lat", "lon", "code", "ne_lat", "ne_lon", "sw_lat", "sw_lon"])
    else:
        writer.writerow(["country", "localities", "address", "confidence", "quality", "lat", "lon", "ne_lat", "ne_lon", "sw_lat", "sw_lon"])

with open("../../inputs/spawning-records.csv", 'r') as fp:
    reader = csv.reader(fp)
    header = next(reader)

    for row in reader:
        key = (row[header.index('country')], row[header.index('localities')])
        if key in known:
            continue

        query = key[1] + ", " + key[0]
        print(query)
        if source == 'geonames':
            g = geocoder.geonames(query, key='jrising', featureClass=['A', 'H', 'L', 'U', 'T', 'S'])
            time.sleep(4)
            if g.geonames_id:
                g2 = geocoder.geonames(g.geonames_id, method='details', key='jrising')
                time.sleep(4)

                if g2.bbox:
                    writer.writerow([key[0], key[1], g.geonames_id, g.address, g.description, g.country, g2.lat, g2.lng, g2.code, g2.bbox['northeast'][0], g2.bbox['northeast'][1], g2.bbox['southwest'][0], g2.bbox['southwest'][1]])
                else:
                    writer.writerow([key[0], key[1], g.geonames_id, g.address, g.description, g.country, g2.lat, g2.lng, g2.code, "NA", "NA", "NA", "NA"])
            else:
                writer.writerow([key[0], key[1], "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"])
        elif source == 'arcgis':
            g = geocoder.arcgis(query)
            time.sleep(4)
            if g and g.json['ok']:
                writer.writerow([key[0], key[1], g.address, g.confidence, g.quality, g.lat, g.lng, g.bbox['northeast'][0], g.bbox['northeast'][1], g.bbox['southwest'][0], g.bbox['southwest'][1]])
            else:
                writer.writerow([key[0], key[1], "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"])
                
        known.add(key)
