#!/usr/bin/env ruby

require 'json'
require 'open3'

class String
  def view_entrypoint
    begin
      result_lines =
        system_lines("./stack exec -- lorentz-contract-view view-to-params --parameterType #{self.inspect}")&.to_a
    rescue RuntimeError
      return nil
    end
    if result_lines.nil?
      return nil
    end
    unless result_lines.length == 2
      raise "Expected exactly 2 lines of output, but got: #{result_lines.inspect}"
    end
    result_lines
  end
end

def fa121_address()
  "KT1RUhPAABRhZBctcsWFtymyjpuBQdLTqaAQ"
end

def system_lines(cmd, allow_failure=false)
  Enumerator.new do |y|
    puts
    puts "Running:"
    puts cmd
    puts
    Open3.popen2e(cmd) do |stdin, stdout, thread|
      while line=stdout.gets do 
        puts(line)
        y << line.chomp
      end
      exit_status = thread.value
      unless exit_status.success? || allow_failure
        raise exit_status.inspect
      end
    end
  end
end

def system_line(cmd)
  if result = system_lines(cmd)
    result.first&.chomp
  else
    result
  end
end

class Enumerator
  def next_nonempty
    while (next_result = self.next).empty?
    end
    next_result
  end

  def get_entrypoints(contract_addr=fa121_address)
    header_line = self.next_nonempty
    header_regexp = /^\s*Entrypoints for contract #{contract_addr}:\s*$/
    unless header_regexp.match(header_line)
      raise "Expected the first line to match: #{header_regexp.inspect}, but found:\n  #{header_line.inspect}"
    end

    entrypoint_regexp = /^\s*(?<entrypoint_name>\w+):\s+(?<entrypoint_type>.*)$/
    Enumerator.new do |y|
      while true do
        begin
          current_line = self.next_nonempty
          unless (match_result = entrypoint_regexp.match(current_line)).nil?
            y << [match_result[:entrypoint_name], match_result[:entrypoint_type].rstrip]
          else
            p current_line
            raise "Expected all but the first line to match: #{entrypoint_regexp.inspect}, but found: #{current_line}"
          end
        rescue StopIteration
          break
        end
      end
    end
  end
end

class TezosClient
  attr_accessor :client_path, :exec_lambda_addr, :user_addr
  @contract_entrypoints = {}

  class << self
    attr_accessor :contract_entrypoints
  end

  def initialize(client_path='tezos-client -A rpcalpha.tzbeta.net -P 443 -S', exec_lambda_addr='KT1NFUsGvAomSSNnKjps8RL1EjGKfWQmM4iw', user_addr='tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr')
    @client_path = client_path
    @exec_lambda_addr = exec_lambda_addr
    @user_addr = user_addr
  end

  def run(cmd, allow_failure=false)
    system_lines "TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=yes #{self.client_path} #{cmd}", allow_failure
  end

  def call_exec_lambda(arg, allow_failure=false)
    self.run "transfer 0 from #{self.user_addr} to #{self.exec_lambda_addr} --arg #{arg.inspect} --dry-run", allow_failure
  end

  def get_entrypoints(contract_addr=fa121_address)
    unless (cached_result = self.class.contract_entrypoints[contract_addr]).nil?
      cached_result
    else
      self.class.contract_entrypoints[contract_addr] =
        self.run("get contract entrypoints for #{contract_addr}").to_enum.get_entrypoints(contract_addr).to_h
      self.get_entrypoints contract_addr
    end
  end

  def get_view_entrypoints(contract_addr=fa121_address)
    self.get_entrypoints.map do |entrypoint_name, entrypoint_type|
      unless (view_entrypoint = entrypoint_type.view_entrypoint).nil?
        [entrypoint_name, view_entrypoint]
      end
    end.select(&:itself).to_h
  end

  def get_typed_lambda_params(contract_addr=fa121_address)
    self.get_view_entrypoints.map do |entrypoint_name, types|
      [entrypoint_name, [types, Proc.new do |param_value|
        lambda_param = self.typed_lambda_param(types, entrypoint_name, param_value, contract_addr)
        puts
        puts
        p lambda_param
        puts
        puts
        self.call_exec_lambda lambda_param, true
      end]]
    end.to_h
  end

  def typed_lambda_param(types, entrypoint_name, param_value, view_contract_addr)
    unless types.nil?
      p types
      param_type, callback_type = types
      view_to_void_cmd =
        [ "./stack exec -- lorentz-contract-view view-to-void ",
          "--execLambda #{self.exec_lambda_addr.inspect}",
          "--parameterType #{param_type.inspect}",
          "--callbackType #{callback_type.inspect}",
          "--viewContract #{view_contract_addr.inspect}",
          "--viewParameter #{param_value.inspect}"
      ].join(' ')
      p view_to_void_cmd
      system_line(view_to_void_cmd).sub(
        '(pair address unit) (pair address unit)',
        '(pair address unit) (pair (list operation) unit)'
      ).sub(
        "CONTRACT (pair #{param_type} (contract #{callback_type}))",
        "CONTRACT %#{entrypoint_name} (pair #{param_type} (contract #{callback_type}))"
      )
    else
      raise "Types are nil"
    end
  end
end

if __FILE__ == $0
  typed_lambda_params = TezosClient.new(
    client_path=ARGV[0],
    exec_lambda_addr=ARGV[1],
    user_addr=ARGV[2]
  ).get_typed_lambda_params(
    contract_addr=ARGV[3]
  )
  puts "Entrypoints for #{ARGV[3]}:"
  puts JSON.pretty_generate(typed_lambda_params.map{|x,y|[x,y[0]]}.to_h)

  entrypoint_name = ARGV[4]
  parameter_value = ARGV[5]

  puts "Calculating result:"
  typed_lambda_params[entrypoint_name]&.[](1)&.[](parameter_value)&.each_cons(2) do |line_pair|
    line = line_pair.join ' '
    if /^script reached FAILWITH instruction with \(Pair\s+(?<returned_value>.*)\s+Unit\)\s*$/ =~ line
      puts
      puts "Result:"
      puts returned_value
      break
    end
  end
end

