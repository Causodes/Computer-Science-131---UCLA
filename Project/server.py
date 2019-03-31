import asyncio
import aiohttp
import sys
import json
import time

API_KEY = 'AIzaSyCzNiZVq-PKdPreHoW2hiHO07-kobnmD60'

server_dic = {
	'Goloman': ['Hands', 'Holiday', 'Wilkes'],
	'Hands': ['Wilkes', 'Goloman'],
	'Holiday' : ['Welsh', 'Wilkes', 'Goloman'],
	'Wilkes' : ['Goloman', 'Hands', 'Holiday'],
	'Welsh' : ['Holiday']
}

port_dic = {
	'Goloman' : 12135,
	'Hands' : 12136,
	'Holiday': 12137,
	'Wilkes': 12138,
	'Welsh' : 12139
}

clients = {}

log_file = ''

def coord_parse(input):
	signs = []
	for i in range(len(input)):
		if input[i] == '+' or input[i] == '-':
			signs.append(i)
	if len(signs) != 2:
		return None
	if signs[0] != 0:
		return None
	if signs[1] == len(input) - 1:
		return None
	lat_long = None
	try:
		lat_long = float(input[:signs[1]]), float(input[signs[1]:])
	except:
		pass
	return lat_long

def input_check(input):
	fixed_input = input.strip().split()
	if len(fixed_input) < 1:
		return -1
	if fixed_input[0] == "IAMAT":
		if len(fixed_input) == 4:
			if coord_parse(fixed_input[2]) is not None:
				time = None
				try:
					time = float(fixed_input[3])
				except:
					pass
				if time is None:
					return -1
				return 1
			else:
				return -1
		else:
			return -1
	elif fixed_input[0] == "WHATSAT":
		if len(fixed_input) == 4:
			radius = None
			try:
				radius = float(fixed_input[2])
			except:
				pass
			if radius is None or radius > 50 or radius <= 0:
				return -1
			else:
				entries = None
				try:
					entries = int(fixed_input[3])
				except:
					pass
				if entries is None or entries > 20 or entries <= 0:
					return -1
				else:
					return 2
		else:
			return -1
	elif fixed_input[0] == "AT":
		if len(fixed_input) == 6:
			return 3
		else:
			return -1
	else:
		return -1

async def flood(input, server_name):
	for server in server_dic[server_name]:
		log_file.write("Attempting Connection with Server {0} at Port {1}...".format(server, port_dic[server]))
		try:
			reader, writer = await asyncio.open_connection('127.0.0.1', port_dic[server], loop=loop)
			writer.write(input.encode())
			await writer.drain()
			writer.close()
			log_file.write("Connection Successful\n")
		except:
			log_file.write("Connection Failed\n")
			pass
		
async def generate_output(input, received_time):
	message_type = input_check(input)
	input_message = input.strip().split()
	out_message = ""
	error_message = "? {0}".format(input_message)
	
	if message_type == 1:
		if coord_parse(input_message[2]) is None:
			out_message = error_message
		else:
			iamat_msg = [input_message[0], input_message[1], input_message[2], input_message[3], str(received_time), sys.argv[1]]
			clients[input_message[1]] = iamat_msg
			time_diff = received_time - float(input_message[3])
			if time_diff >= 0:
				time_diff = "+" + str(time_diff)
			else:
				time_diff = "-" + str(time_diff)
			out_message = ("AT {0} {1} {2}\n".format(sys.argv[1], time_diff, ' '.join(input_message[1:])))
			asyncio.ensure_future(flood('AT {0}\n'.format(' '.join(iamat_msg[1:])), sys.argv[1]))
	elif message_type == 2:
		if input_message[1] not in clients:
			out_message = error_message
		else:
			client = clients[input_message[1]]
			location = coord_parse(client[2])
			location = str(location[0]) + "," + str(location[1])
			radius = float(input_message[2]) * 1000
			url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1}&radius={2}'.format(API_KEY, location, radius)
			time_diff = float(client[4]) - float(client[3])
			if time_diff >= 0:
				time_diff = "+" + str(time_diff)
			else:
				time_diff = "-" + str(time_diff)
			out_message = "AT {0} {1} {2} {3} {4}\n".format(client[5], time_diff, client[1], client[2], client[3])
			async with aiohttp.ClientSession() as session:
				async with session.get(url) as resp:
					response = await resp.json()
					response['results'] = response['results'][:int(input_message[3])]
					out_message += json.dumps(response, indent = 3)
					out_message += "\n\n"
	else:
		out_message = error_message
	return out_message
	
async def handle_input(reader, writer):
	data = await reader.readline()
	time_received = time.time()
	in_message = data.decode()
	log_file.write("RECEIVED: " + in_message)
	
	fixed_message = in_message.strip().split()
	
	if fixed_message[0] == "AT" and input_check(in_message):
		if fixed_message[1] not in clients:
			clients[fixed_message[1]] = fixed_message
			asyncio.ensure_future(flood('AT {0}\n'.format(' '.join(fixed_message[1:])), sys.argv[1]))
		else:
			if fixed_message[3] > clients[fixed_message[1]][3]:
				clients[fixed_message[1]] = fixed_message
				asyncio.ensure_future(flood('AT {0}\n'.format(' '.join(fixed_message[1:])), sys.argv[1]))
	else:
		out_message = await generate_output(in_message, time_received)
		log_file.write("SENDING: " + out_message)
		writer.write(out_message.encode())
		await writer.drain()
	
def main():
	if len(sys.argv) != 2:
		print("Bad args")
		sys.exit(1)
	if sys.argv[1] not in port_dic:
		print("Bad server name")
		sys.exit(1)
		
	global log_file
	log_file = open(sys.argv[1] + "_log.txt", "w+")
	
	global loop
	loop = asyncio.get_event_loop()
	coro = asyncio.start_server(handle_input, '127.0.0.1', port_dic[sys.argv[1]], loop=loop )
	server = loop.run_until_complete(coro)
	
	try:
		loop.run_forever()
	except KeyboardInterrupt:
		pass
	
	server.close()
	loop.run_until_complete(server.wait_closed())
	loop.close()
	log_file.close()
	
if __name__ == '__main__':
	main()