import argparse
import csv
import numpy as np
import igraph
from tqdm import tqdm
import ray

from typing import Any, List

edge_score_type = Any


def single_edge_interaction_score(max_path_length: int, graph: igraph.Graph, edge: List[str]):
    """Computes interaction score for a single edge and returns it.

    Arguments:
    max_path_length -- maximum length of simple paths to include in computation
    graph -- igraph graph object to compute interaction score for
    edge -- single line of edge list file

    Returns:
    Tuple with source, target and interaction score
    """
    sums_of_weight_products = [0] * max_path_length
    num_paths = [0] * max_path_length
    simple_paths = graph.get_all_simple_paths(edge[0], edge[1], cutoff=max_path_length)
    for path in simple_paths:
        path_length = len(path) - 1
        sums_of_weight_products[path_length - 1] += np.prod([graph.es.find(_source=path[i], _target=path[i+1])["weight"] for i in range(path_length)])
        num_paths[path_length - 1] += 1
    sums_of_weight_products = np.array(sums_of_weight_products, dtype=float)
    num_paths = np.array(num_paths, dtype=float)
    normalized_sums = np.divide(sums_of_weight_products, num_paths, out=np.zeros_like(num_paths), where=num_paths != 0)
    edge_score = edge[0], edge[1], np.sum(normalized_sums)
    return edge_score


def add_score_attribute_to_edge(graph: igraph.Graph, edge_score: edge_score_type, score_name: str):
    """Fills in a score edge attribute value for a single edge.

    Arguments:
    graph -- igraph graph object
    edge_score -- Tuple with source, target and score value
    score_name -- name of the attribute

    No return value
    """
    graph.es.find(_source=edge_score[0], _target=edge_score[1])[score_name] = edge_score[2]


def sequential_interaction_score(max_path_length: int, graph: igraph.Graph, edge_list_reader: csv.reader, num_edges: int):
    """Computes interaction score sequentially and adds it to graph as attribute.

    Arguments:
    max_path_length -- maximum length of simple paths to include in computation
    graph -- igraph graph object to compute interaction score for
    edge_list_reader -- iterator over the lines of an edge list file containing the edges to compute interaction score for

    No return value
    """
    progress_bar = tqdm(desc="Calculating interaction score", total=num_edges)
    for edge in edge_list_reader:
        edge_score = single_edge_interaction_score(max_path_length, graph, edge)
        add_score_attribute_to_edge(graph, edge_score, "interaction_weight")
        progress_bar.update()
    progress_bar.close()


def distributed_interaction_score(max_path_length: int, graph: igraph.Graph, edge_list_reader: csv.reader, num_cpus: int, cluster_address: str = "auto"):
    """Computes interaction score in parallel using a ray cluster and adds it to graph as attribute.

    Arguments:
    max_path_length -- maximum length of simple paths to include in computation
    graph -- igraph graph object to compute interaction score for
    edge_list_reader -- iterator over the lines of an edge list file containing the edges to compute interaction score for

    Optional:
    cluster_address -- IP address of existing ray cluster to be used. If None it creates a local cluster (default).

    No return value
    """

    parallel_single_edge_interaction_score = ray.remote(single_edge_interaction_score)

    if cluster_address == "auto":
        ray.init(include_dashboard=False, num_cpus=num_cpus)
    else:
        ray.init(address=cluster_address, include_dashboard=False)
    remote_graph = ray.put(graph)
    futures = [parallel_single_edge_interaction_score.remote(max_path_length, remote_graph, edge) for edge in
               edge_list_reader]
    progress_bar = tqdm(desc="Calculating interaction score", total=len(futures))
    while len(futures):
        [done], futures = ray.wait(futures)
        add_score_attribute_to_edge(graph, ray.get(done), "interaction_weight")
        progress_bar.update()
    progress_bar.close()
    ray.shutdown()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("graph_file", help="file containing the input graph in gml format")
    parser.add_argument("edge_list_file", help="file containing the list of edges to calculate the interaction score for in tsv format (source,target,weight)")
    parser.add_argument("max_path_length", default=3, help="maximum length of simple paths considered for score computation")
    parser.add_argument("--output", default="int_score_graph.gml", help="output file name, graph with score in gml format")
    parser.add_argument("--num_cpus", default=1, help="number of cpus to use for parallel computation")
    parser.add_argument("--cluster_address", default="auto", help="address of ray cluster, if a cluster is used")
    parser.add_argument("--distributed", action="store_const", const=True, default=False, help="run in parallel with ray")
    args = parser.parse_args()

    graph = igraph.read(args.graph_file, format="gml")
    max_path_length = int(args.max_path_length)
    num_cpus = int(args.num_cpus)

    with open(args.edge_list_file, "r") as el_file:
        edge_list_reader = csv.reader(el_file, delimiter="\t")
        next(edge_list_reader)  # skip header
        if args.distributed and num_cpus > 1:
            distributed_interaction_score(max_path_length, graph, edge_list_reader, num_cpus, args.cluster_address)
        else:
            with open(args.edge_list_file, "r") as temp_el_file:
                num_edges = sum(1 for _ in temp_el_file)
            sequential_interaction_score(max_path_length, graph, edge_list_reader, num_edges)
    graph.save(args.output, format="gml")


if __name__ == "__main__":
    main()
